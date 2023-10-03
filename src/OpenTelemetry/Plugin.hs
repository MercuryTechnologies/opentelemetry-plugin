{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-| This module provides a GHC plugin that will export open telemetry metrics
    for your build.  Specifically, this plugin will create one span per module
    (recording how long that module took to build) and one sub-span per phase
    of that module's build (recording how long that phase took).
-}
module OpenTelemetry.Plugin
    ( -- * Plugin
      plugin
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import GHC.Types.Target (Target(..), TargetId(..))
import OpenTelemetry.Context (Context)

import GHC.Plugins
    ( CoreToDo(..)
    , GenModule(..)
    , HscEnv(..)
    , Plugin(..)
    )
import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified GHC.Plugins as Plugins
import qualified GHC.Utils.Outputable as Outputable
import qualified OpenTelemetry.Plugin.Shared as Shared

wrapTodo :: MonadIO io => IO Context -> CoreToDo -> io CoreToDo
wrapTodo getParentContext todo =
    case todo of
        CoreDoPasses passes ->
            fmap CoreDoPasses (traverse (wrapTodo getParentContext) passes)

        _ -> liftIO do
            let sdoc = Outputable.ppr todo

            let label =
                    Outputable.showSDocOneLine Outputable.defaultSDocContext sdoc

            (_, beginPass, endPass) <- do
                Shared.makeWrapperPluginPasses False getParentContext (Text.pack label)

            let beginPluginPass =
                    CoreDoPluginPass ("begin " <> label) \modGuts -> liftIO do
                        beginPass

                        pure modGuts

            let endPluginPass =
                    CoreDoPluginPass ("end " <> label) \modGuts -> liftIO do
                        endPass

                        pure modGuts

            pure (CoreDoPasses [ beginPluginPass, todo, endPluginPass ])

-- | GHC plugin that exports open telemetry metrics about the build
plugin :: Plugin
plugin =
    Plugins.defaultPlugin
        { driverPlugin
        , pluginRecompile
        , installCoreToDos
        }
  where
    driverPlugin _ hscEnv@HscEnv{ hsc_targets } = do
        let rootModuleNames = do
                Target{ targetId = TargetModule rootModuleName } <- hsc_targets

                pure (Plugins.moduleNameString rootModuleName)

        Shared.setRootModuleNames rootModuleNames

        Shared.initializeTopLevelContext

        pure hscEnv

    installCoreToDos _ todos = do
        module_ <- Plugins.getModule

        let moduleName_ = moduleName module_

        let moduleText = Text.pack (Plugins.moduleNameString moduleName_)

        (getCurrentContext, firstPluginPass, lastPluginPass) <- do
            liftIO (Shared.makeWrapperPluginPasses True Shared.getTopLevelContext moduleText)

        let firstPass =
                CoreDoPluginPass "begin module" \modGuts -> liftIO do
                    firstPluginPass

                    pure modGuts

        let lastPass =
                CoreDoPluginPass "end module" \modGuts -> liftIO do
                    lastPluginPass

                    isRoot <- Shared.isRootModule (Plugins.moduleNameString moduleName_)

                    -- Flush metrics if we're compiling one of the root
                    -- modules.  This is to work around the fact that we don't
                    -- have a proper way to finalize the `TracerProvider`
                    -- (since the finalizer would normally be responsible for
                    -- flushing any last metrics).
                    --
                    -- You might wonder: why don't we end the top-level span
                    -- here?  Well, we don't know which one of the root modules
                    -- will be the last one to be compiled.  However, flushing
                    -- once per root module is still fine because flushing is
                    -- safe to run at any time and in practice there will only
                    -- be a few root modules.
                    Monad.when isRoot Shared.flush

                    pure modGuts

        newTodos <- traverse (wrapTodo getCurrentContext) todos

        pure ([ firstPass ] <> newTodos <> [ lastPass ])

    pluginRecompile = Plugins.purePlugin