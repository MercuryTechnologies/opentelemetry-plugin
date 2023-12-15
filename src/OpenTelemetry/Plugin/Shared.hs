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
    , getPluginShouldRecordPasses

      -- * Top-level context
    , initializeTopLevelContext
    , getTopLevelContext

      -- * Root module names
    , setRootModuleNames
    , isRootModule

      -- * Flushing
    , flush

    , getSampler
    , tracer
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Text (Text)
import OpenTelemetry.Context (Context)
import OpenTelemetry.Trace.Sampler (Sampler(..), SamplingResult(..))
import Prelude hiding (span)
import System.Random.MWC (GenIO)

import OpenTelemetry.Trace
    ( Attribute(..)
    , PrimitiveAttribute(..)
    , InstrumentationLibrary(..)
    , Span
    , SpanArguments(..)
    , SpanContext(..)
    , Tracer
    , TracerProvider
    , TracerProviderOptions(..)
    )

import qualified Control.Monad as Monad
import qualified Control.Concurrent.MVar as MVar
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Version as Version
import qualified OpenTelemetry.Context as Context
import qualified OpenTelemetry.Propagator.W3CBaggage as W3CBaggage
import qualified OpenTelemetry.Propagator.W3CTraceContext as W3CTraceContext
import qualified OpenTelemetry.Trace as Trace
import qualified OpenTelemetry.Trace.Core as Trace.Core
import qualified OpenTelemetry.Trace.Sampler as Sampler
import qualified OpenTelemetry.Trace.TraceState as TraceState
import qualified Paths_opentelemetry_plugin as Paths
import qualified System.Environment as Environment
import qualified System.IO.Unsafe as Unsafe
import qualified System.Random.MWC as MWC
import qualified Text.Read as Read

{-| Very large Haskell builds can generate an enormous number of spans,
    but none of the stock samplers provide a way to sample a subset of
    the `Span`s within a trace.

    This adds a new "spanratio" sampler which can be used to sample subset of
    module spans.
-}
getSampler :: IO (Maybe Sampler)
getSampler = do
    maybeSampler <- Environment.lookupEnv "OTEL_TRACES_SAMPLER"

    maybeRatio <- Environment.lookupEnv "OTEL_TRACES_SAMPLER_ARG"

    pure do
        "spanratio" <- maybeSampler
        ratioString <- maybeRatio
        ratio <- Read.readMaybe ratioString
        pure (spanRatioBased ratio)

{-| Like a lot of other uses of `Unsafe.unsafePerformIO` in this module, we're
    doing this because the plugin interface doesn't provide a way for us to
    acquire resources before returning the plugin.
-}
generator :: GenIO
generator = Unsafe.unsafePerformIO MWC.createSystemRandom
{-# NOINLINE generator #-}

spanRatioBased :: Double -> Sampler
spanRatioBased fraction = Sampler
    { getDescription =
          "SpanRatioBased{" <> Text.pack (show fraction) <> "}"
    , shouldSample = \context traceId_ name spanArguments -> do
        case HashMap.lookup "sample" (attributes spanArguments) of
            Just (AttributeValue (BoolAttribute True)) -> do
                random <- MWC.uniformR (0, 1) generator

                let samplingResult =
                        if random < fraction then RecordAndSample else Drop

                traceState_ <- case Context.lookupSpan context of
                    Nothing ->
                        pure TraceState.empty

                    Just span ->
                        fmap traceState (Trace.Core.getSpanContext span)

                pure (samplingResult, HashMap.empty, traceState_)

            _ ->
                shouldSample Sampler.alwaysOn context traceId_ name spanArguments
    }

{-| Note: We don't properly shut this down using `Trace.shutdownTracerProvider`,
    but all that the shutdown does is flush metrics, so instead we flush metrics
    (using `flush`) at the end of compilation to make up for the lack of a
    proper shutdown.
-}
tracerProvider :: TracerProvider
tracerProvider = Unsafe.unsafePerformIO do
    (processors, options) <-
        -- This function will collect *all* of the command line arguments
        -- that were provided to GHC. This results in a huge amount of data
        -- being sent. For that reason, we blank out the process arguments
        -- for this section of code.
        Environment.withArgs [] do
            Trace.getTracerProviderInitializationOptions

    maybeSampler <- getSampler

    let newOptions =
            case maybeSampler of
                Nothing      -> options
                Just sampler -> options{ tracerProviderOptionsSampler = sampler }

    tracerProvider_ <- Trace.createTracerProvider processors newOptions

    Trace.setGlobalTracerProvider tracerProvider_

    pure tracerProvider_
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
    :: Bool
      -- ^ Whether to sample a subset of spans
    -> IO Context
       -- ^ Action to read the parent span's `Context`
    -> Text
       -- ^ Label for the current span
    -> IO (IO Context, IO (), IO ())
makeWrapperPluginPasses sample getParentContext label = liftIO do
    spanMVar           <- liftIO MVar.newEmptyMVar
    currentContextMVar <- liftIO MVar.newEmptyMVar

    let beginPass = do
            parentContext <- getParentContext

            let spanArguments =
                    if sample
                    then
                        Trace.defaultSpanArguments
                            { attributes =
                                HashMap.singleton "sample" (AttributeValue (BoolAttribute True))
                            }
                    else
                        Trace.defaultSpanArguments

            passSpan <- Trace.createSpan tracer parentContext ("OG: " <> label) spanArguments

            _ <- MVar.tryPutMVar spanMVar passSpan

            let currentContext = Context.insertSpan passSpan parentContext

            _ <- MVar.tryPutMVar currentContextMVar currentContext

            pure ()

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
    traceState_ <- lookupEnv "TRACESTATE"

    case W3CTraceContext.decodeSpanContext traceParent traceState_ of
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

    _ <- MVar.tryPutMVar topLevelContextMVar contextWithSpan

    return ()

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

    _ <- MVar.tryPutMVar rootModuleNamesMVar set

    pure ()

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

-- | Returns 'True' if the plugin should create spans for module passes in
-- compilation. Examples would be Simplifier, any other plugin execution,
-- etc.
getPluginShouldRecordPasses :: IO Bool
getPluginShouldRecordPasses = do
    maybeRecordPasses <- Environment.lookupEnv "OTEL_GHC_PLUGIN_RECORD_PASSES"
    pure $ Maybe.fromMaybe False do
        recordPasses <- maybeRecordPasses
        pure $ Text.toLower (Text.pack recordPasses) `elem` ["true", "t"]
