{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

{-| This module provides the GHC-API-agnostic logic for this plugin (mostly
    open telemetry utilities)

    Because of how GHC plugins work, this module has to do some evil stuff
    under the hood to work within the confines of the plugin API.  That means
    that you should take care to use the utilities in this module correctly
    in order to avoid the plugin hanging.
-}
module OpenTelemetry.Plugin.Shared
    ( -- * Plugin passes
      makeWrapperPluginPasses

      -- * Top-level context
    , initializeTopLevelContext
    , getTopLevelContext

      -- * Root module names
    , setRootModuleNames
    , isRootModule

      -- * Flushing
    , flush
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Text (Text)
import OpenTelemetry.Context (Context)
import Prelude hiding (span)

import OpenTelemetry.Trace
    ( InstrumentationLibrary(..)
    , Span
    , SpanArguments(..)
    , Tracer
    , TracerProvider
    )

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Version as Version
import qualified OpenTelemetry.Context as Context
import qualified OpenTelemetry.Propagator.W3CBaggage as W3CBaggage
import qualified OpenTelemetry.Propagator.W3CTraceContext as W3CTraceContext
import qualified OpenTelemetry.Trace as Trace
import qualified OpenTelemetry.Trace.Core as Trace.Core
import qualified Paths_opentelemetry_plugin as Paths
import qualified System.Environment as Environment
import qualified System.IO.Unsafe as Unsafe

{-| Note: We don't properly shut this down using `shutdownTracerProvider`, but
    all that the shutdown does is flush metrics, so instead we flush metrics
    (using `flush`) at the end of compilation to make up for the lack of a
    proper shutdown.
-}
tracerProvider :: TracerProvider
tracerProvider = Unsafe.unsafePerformIO Trace.initializeGlobalTracerProvider
{-# NOINLINE tracerProvider #-}

tracer :: Tracer
tracer =
    Trace.makeTracer tracerProvider instrumentationLibrary Trace.tracerOptions
  where
    instrumentationLibrary =
        InstrumentationLibrary
            { libraryName    = "opentelemetry-plugin"
            , libraryVersion = Text.pack (Version.showVersion Paths.version)
            }

{-| This used by the GHC plugin to create two plugin passes that start and stop
    a `Span`, respectively.

    In order for `Span` ancestry to be tracked correctly this takes an
    @`IO` `Context`@ as an input (to read the parent `Span`'s `Context`) and
    returns an @`IO` `Context`@ as an output (to read the current `Span`'s
    `Context`).
-}
makeWrapperPluginPasses
    :: IO Context
       -- ^ Action to ead the parent span's `Context`
    -> Text
       -- ^ Label for the current span
    -> IO (IO Context, IO (), IO ())
makeWrapperPluginPasses getParentContext label = liftIO do
    spanMVar           <- liftIO MVar.newEmptyMVar
    currentContextMVar <- liftIO MVar.newEmptyMVar

    let beginPass = do
            parentContext <- getParentContext

            passSpan <- Trace.createSpan tracer parentContext label Trace.defaultSpanArguments

            MVar.putMVar spanMVar passSpan

            let currentContext = Context.insertSpan passSpan parentContext

            MVar.putMVar currentContextMVar currentContext

    let endPass = do
            passSpan <- MVar.readMVar spanMVar

            Trace.endSpan passSpan Nothing

    pure (MVar.readMVar currentContextMVar, beginPass, endPass)

{-| We're intentionally **NOT** using `OpenTelemetry.Context.ThreadLocal`
    here since the `GHC.Plugins.Plugin` logic doesn't necessarily run in a
    single thread (@ghc@ builds can be multi-threaded).  Instead, we provide
    our own `Context` global variable.
-}
topLevelContextMVar :: MVar Context
topLevelContextMVar = Unsafe.unsafePerformIO MVar.newEmptyMVar
{-# NOINLINE topLevelContextMVar #-}

getTopLevelSpan :: IO Span
getTopLevelSpan = do
    traceParent <- lookupEnv "TRACEPARENT"
    traceState  <- lookupEnv "TRACESTATE"

    case W3CTraceContext.decodeSpanContext traceParent traceState of
        Just spanContext ->
            pure (Trace.Core.wrapSpanContext spanContext)

        Nothing -> do
            -- If we're not inheriting a span from
            -- `TRACEPARENT`/`TRACESTATE`, then create a zero-duration span
            -- whose sole purpose is to be a parent span for each module's
            -- spans.
            --
            -- Ideally we'd like this span's duration to last for the
            -- entirety of compilation, but there isn't a good way to end
            -- the span when compilation is done.  Also, we still need
            -- *some* parent span for each module's spans, otherwise an
            -- entirely new trace will be created for each new span.
            -- Creating a zero-duration span is the least-worst solution.
            --
            -- Note that there aren't any issues with the child spans
            -- lasting longer than the parent span.  This is supported by
            -- open telemetry and the Haskell API.
            timestamp <- Trace.Core.getTimestamp

            let arguments =
                    Trace.defaultSpanArguments
                        { startTime = Just timestamp }

            span <- Trace.createSpan tracer Context.empty "opentelemetry GHC plugin" arguments

            Trace.endSpan span (Just timestamp)

            pure span

getTopLevelBaggage :: IO Context
getTopLevelBaggage = do
    maybeBytes <- lookupEnv "BAGGAGE"
    case maybeBytes >>= W3CBaggage.decodeBaggage of
        Nothing      -> pure Context.empty
        Just baggage -> pure (Context.insertBaggage baggage Context.empty)

lookupEnv :: String -> IO (Maybe ByteString)
lookupEnv = fmap (fmap (fmap encode)) Environment.lookupEnv
  where
    encode = Text.Encoding.encodeUtf8 . Text.pack

{-| This initializes the top-level `Context` using the @TRACEPARENT@ \/
    @TRACESTATE@ \/ @BAGGAGE@ environment variables (if present) and otherwise
    sets it to the empty `Context`

    You have to run this command before calling `getTopLevelContext` otherwise
    the latter will hang.
-}
initializeTopLevelContext :: IO ()
initializeTopLevelContext = do
    span <- getTopLevelSpan

    context <- getTopLevelBaggage

    let contextWithSpan = Context.insertSpan span context

    MVar.putMVar topLevelContextMVar contextWithSpan

-- | Access the top-level `Context` computed by `initializeTopLevelContext`
getTopLevelContext :: IO Context
getTopLevelContext = MVar.readMVar topLevelContextMVar

{-| This is used for communicating between `GHC.Plugins.driverPlugin` and
    `GHC.Plugins.installCoreToDos`, because only `GHC.Plugins.driverPlugin` has
    access to the full module graph, but there isn't a good way within the
    `GHC.Plugins.Plugin` API to share that information with the rest of the
    plugin other than a global variable.
-}
rootModuleNamesMVar :: MVar (Set Text)
rootModuleNamesMVar = Unsafe.unsafePerformIO MVar.newEmptyMVar
{-# NOINLINE rootModuleNamesMVar #-}

{-| Set the root module names (computed by `GHC.Plugins.driverPlugin`)

    You have to run this command before calling `isRootModule` otherwise
    the latter will hang.
-}
setRootModuleNames :: [String] -> IO ()
setRootModuleNames rootModuleNames = do
    let set = Set.fromList (map Text.pack rootModuleNames)

    MVar.putMVar rootModuleNamesMVar set

-- | Check if a module is one of the root modules
isRootModule :: String -> IO Bool
isRootModule moduleName = do
    rootModuleNames <- MVar.readMVar rootModuleNamesMVar

    pure (Set.member (Text.pack moduleName) rootModuleNames)

-- | Flush all metrics
flush :: IO ()
flush = do
    _ <- Trace.Core.forceFlushTracerProvider tracerProvider Nothing
    -- We can't check the result yet because
    -- `FlushResult` is not exported by
    -- `hs-opentelemetry-api`
    --
    -- https://github.com/iand675/hs-opentelemetry/pull/96
    _ <- Trace.Core.forceFlushTracerProvider tracerProvider Nothing

    pure ()
