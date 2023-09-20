{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Plugin
    ( -- * Plugin
      plugin
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Set (Set)
import Data.Text (Text)
import GHC.Types.Target (Target(..), TargetId(..))
import OpenTelemetry.Context (Context)
import Prelude hiding (span)

import GHC.Plugins
    ( CorePluginPass
    , CoreToDo(..)
    , GenModule(..)
    , HscEnv(..)
    , ModuleName(..)
    , Plugin(..)
    )
import OpenTelemetry.Trace
    ( InstrumentationLibrary(..)
    , SpanArguments(..)
    , Tracer
    , TracerProvider
    )

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified GHC.Plugins as Plugins
import qualified GHC.Utils.Outputable as Outputable
import qualified OpenTelemetry.Context as Context
import qualified OpenTelemetry.Trace as Trace
import qualified OpenTelemetry.Trace.Core as Trace.Core
import qualified Paths_opentelemetry_plugin as Paths
import qualified System.IO.Unsafe as Unsafe

{-| Note: We don't properly shut this down using `shutdownTracerProvider`, but
    all that the shutdown does is flush metrics, so instead we flush metrics
    at the end of compilation to make up for the lack of a proper shutdown.
-}
tracerProvider :: TracerProvider
tracerProvider = Unsafe.unsafePerformIO Trace.initializeGlobalTracerProvider
{-# NOINLINE tracerProvider #-}

{-| This is used for communicating between `driverPlugin` and
    `installCoreToDos`, because only `driverPlugin` has access to the full
    module graph, but there isn't a good way within the `Plugin` API for them
    to share that information other than a global variable.
-}
rootModuleNamesMVar :: MVar (Set ModuleName)
rootModuleNamesMVar = Unsafe.unsafePerformIO MVar.newEmptyMVar
{-# NOINLINE rootModuleNamesMVar #-}

{-| We're intentionally **NOT** using `OpenTelemetry.Context.ThreadLocal`
    here since the `Plugin` logic doesn't necessarily run in a single thread
    (`ghc` builds can be multi-threaded).  Instead, we provide our own
    `Context` global variable.
-}
topLevelContextMVar :: MVar Context
topLevelContextMVar = Unsafe.unsafePerformIO MVar.newEmptyMVar
{-# NOINLINE topLevelContextMVar #-}

tracer :: Tracer
tracer =
    Trace.makeTracer tracerProvider instrumentationLibrary Trace.tracerOptions
  where
    instrumentationLibrary =
        InstrumentationLibrary
            { libraryName    = "opentelemetry-plugin"
            , libraryVersion = Text.pack (Version.showVersion Paths.version)
            }

makeWrapperPluginPasses
    :: MonadIO io
    => IO Context
    -> Text
    -> io (IO Context, CorePluginPass, CorePluginPass)
makeWrapperPluginPasses getParentContext label = liftIO do
    spanMVar           <- liftIO MVar.newEmptyMVar
    currentContextMVar <- liftIO MVar.newEmptyMVar

    let beginPass modGuts = liftIO do
            parentContext <- getParentContext

            passSpan <- Trace.createSpan tracer parentContext label Trace.defaultSpanArguments

            MVar.putMVar spanMVar passSpan

            let currentContext = Context.insertSpan passSpan parentContext

            MVar.putMVar currentContextMVar currentContext

            pure modGuts

    let endPass modGuts = liftIO do
            passSpan <- MVar.readMVar spanMVar

            Trace.endSpan passSpan Nothing

            pure modGuts

    pure (MVar.readMVar currentContextMVar, beginPass, endPass)

wrapTodo :: MonadIO io => IO Context -> CoreToDo -> io CoreToDo
wrapTodo getParentContext todo =
    case todo of
        CoreDoPasses passes ->
            fmap CoreDoPasses (traverse (wrapTodo getParentContext) passes)

        _ -> do
            let sdoc = Outputable.ppr todo

            let label =
                    Outputable.showSDocOneLine Outputable.defaultSDocContext sdoc

            (_, beginPass, endPass) <- do
                makeWrapperPluginPasses getParentContext (Text.pack label)

            let beginPluginPass =
                    CoreDoPluginPass ("begin " <> label) beginPass

            let endPluginPass =
                    CoreDoPluginPass ("end " <> label) endPass

            pure (CoreDoPasses [ beginPluginPass, todo, endPluginPass ])

plugin :: Plugin
plugin =
    Plugins.defaultPlugin
        { driverPlugin
        , pluginRecompile
        , installCoreToDos
        }
  where
    driverPlugin _ hscEnv@HscEnv{ hsc_targets } = do
        let rootModuleNames = Set.fromList do
                Target{ targetId = TargetModule rootModuleName } <- hsc_targets

                pure rootModuleName

        MVar.putMVar rootModuleNamesMVar rootModuleNames

        timestamp <- Trace.Core.getTimestamp

        let arguments =
                Trace.defaultSpanArguments
                    { startTime = Just timestamp }

        -- Intentionally create a zero-duration span whose sole purpose is to
        -- be a parent span for each module's spans.
        --
        -- Ideally we'd like this span's duration to last for the entirety of
        -- compilation, but there isn't a good way to end the span when
        -- compilation is done.  Also, we still need *some* parent span for
        -- each module's spans, otherwise an entirely new trace will be created
        -- for each new span.  Creating a zero-duration span is the least-worst
        -- solution.
        --
        -- Note that there aren't any issues with the child spans lasting
        -- longer than the parent span.  This is supported by open telemetry
        -- and the Haskell API.
        span <- Trace.createSpan tracer Context.empty "opentelemetry GHC plugin" arguments

        let topLevelContext = Context.insertSpan span Context.empty

        MVar.putMVar topLevelContextMVar topLevelContext

        Trace.endSpan span (Just timestamp)

        pure hscEnv

    installCoreToDos _ todos = do
        module_ <- Plugins.getModule

        let moduleName_ = moduleName module_

        let moduleText = Text.pack (Plugins.moduleNameString moduleName_)

        (getCurrentContext, firstPluginPass, lastPluginPass) <- do
            makeWrapperPluginPasses (MVar.readMVar topLevelContextMVar) moduleText

        let firstPass = CoreDoPluginPass "begin module" firstPluginPass

        let lastPass =
                CoreDoPluginPass "end module" \modGuts -> do
                    newModGuts <- lastPluginPass modGuts

                    liftIO do
                        rootModuleNames <- MVar.readMVar rootModuleNamesMVar

                        -- Flush metrics if we're compiling one of the root
                        -- modules.  This is to work around the fact that we
                        -- don't have a proper way to finalize the
                        -- `TracerProvider` (since the finalizer would normally
                        -- be responsible for flushing any last metrics).
                        --
                        -- You might wonder: why don't we end the top-level
                        -- span here?  Well, we don't know which one of the root
                        -- modules will be the last one to be compiled.
                        -- However, flushing once per root module is still fine
                        -- because flushing is safe to run at any time and in
                        -- practice there will only be a few root modules.
                        Monad.when (Set.member moduleName_ rootModuleNames) do
                            -- We can't check the result yet because
                            -- `FlushResult` is not exported by
                            -- `hs-opentelemetry-api`
                            --
                            -- https://github.com/iand675/hs-opentelemetry/pull/96
                            _ <- Trace.Core.forceFlushTracerProvider tracerProvider Nothing

                            pure ()

                    pure newModGuts

        newTodos <- traverse (wrapTodo getCurrentContext) todos

        pure ([ firstPass ] <> newTodos <> [ lastPass ])

    pluginRecompile = Plugins.purePlugin
