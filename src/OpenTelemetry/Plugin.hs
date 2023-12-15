{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

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

import GHC.Driver.Pipeline (runPhase)
import GHC.Driver.Pipeline.Phases
  ( PhaseHook (..),
    TPhase (..),
  )
import GHC.Driver.Hooks (Hooks (..))
import GHC.Plugins
    ( CoreToDo(..)
    , HscEnv(..)
    , Plugin(..)
    )
import qualified Data.Text as Text
import qualified GHC.Plugins as Plugins
import qualified GHC.Utils.Outputable as Outputable
import qualified OpenTelemetry.Plugin.Shared as Shared
import qualified GHC.Driver.Backend as Backend

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

        let closePhase =
                case Backend.backendWritesFiles $ Plugins.backend $ hsc_dflags hscEnv of
                    False ->
                        CloseInHscBackend
                    True ->
                        CloseInMergeForeign

        Shared.setRootModuleNames rootModuleNames

        Shared.initializeTopLevelContext

        pure hscEnv
            { hsc_hooks =
                (hsc_hooks hscEnv)
                    { runPhaseHook =
                        Just $ PhaseHook \phase -> do
                            case phase of
                                T_Hsc _ modSummary -> do
                                    Shared.recordModuleStart modSummary
                                    runPhase phase
                                T_MergeForeign _pipeEnv _hscEnv objectFilePath _filePaths -> do
                                    -- this phase appears to only be run
                                    -- during compilation, not ghci
                                    x <- runPhase phase
                                    case closePhase of
                                        CloseInMergeForeign ->
                                            Shared.recordModuleEndFromObjectFilepath objectFilePath
                                        _ ->
                                            pure ()
                                    pure x
                                T_HscBackend _pipeEnv _hscEnv modName _hscSrc _modLoc _hscAction  -> do
                                    -- this happens in ghci for sure as
                                    -- a last step
                                    x <- runPhase phase
                                    case closePhase of
                                        CloseInHscBackend ->
                                            Shared.recordModuleEndFromModuleName modName
                                        _ ->
                                            pure ()
                                    pure x
                                _ -> do
                                    print (tphase2Text phase, envFromTPhase phase)

                                    runPhase phase
                    }
            }

    installCoreToDos _ todos = do
        shouldMakeSubPasses <- liftIO Shared.getPluginShouldRecordPasses

        if shouldMakeSubPasses
            then do
                module_ <- Plugins.getModule

                (getCurrentContext, firstPluginPass, lastPluginPass) <- do
                    let getContext =
                            Shared.modifyContextWithParentSpan module_ Shared.getTopLevelContext
                    liftIO (Shared.makeWrapperPluginPasses True getContext "CoreToDos")

                let firstPass =
                        CoreDoPluginPass "begin module" \modGuts -> liftIO do
                            firstPluginPass

                            pure modGuts

                let lastPass =
                        CoreDoPluginPass "end module" \modGuts -> liftIO do
                            lastPluginPass

                            pure modGuts

                newTodos <- traverse (wrapTodo getCurrentContext) todos
                pure ([ firstPass ] <> newTodos <> [ lastPass ])
            else do
                pure todos

    pluginRecompile = Plugins.purePlugin

data ClosePhase = CloseInHscBackend | CloseInMergeForeign

tphase2Text :: TPhase res -> String
tphase2Text p =
  case p of
    T_Unlit {} -> "T_Unlit"
    T_FileArgs {} -> "T_FileArgs"
    T_Cpp {} -> "T_Cpp"
    T_HsPp {} -> "T_HsPp"
    T_HscRecomp {} -> "T_HsRecomp"
    T_Hsc {} -> "T_Hsc"
    T_HscPostTc {} -> "T_HscPostTc"
    T_HscBackend {} -> "T_HscBackend"
    T_CmmCpp {} -> "T_CmmCpp"
    T_Cmm {} -> "T_Cmm"
    T_Cc {} -> "T_Cc"
    T_As {} -> "T_As"
    T_LlvmOpt {} -> "T_LlvmOpt"
    T_LlvmLlc {} -> "T_LlvmLlc"
    T_LlvmMangle {} -> "T_LlvmMangle"
    T_MergeForeign {} -> "T_MergeForeign"
    _ -> "Unknown Phase"

-- return an ident of some sort for the phase
envFromTPhase :: TPhase res -> Maybe String
envFromTPhase p =
  case p of
    T_Unlit _ _ _ -> Nothing
    T_FileArgs _ _ -> Nothing
    T_Cpp _ _ _ -> Nothing
    T_HsPp _ _ _ _ -> Nothing
    T_HscRecomp _ _ filepath _ -> Just filepath
    T_Hsc _ modSummary -> Just (Shared.getModuleNameFromSummary modSummary)
    T_HscPostTc _ modSummary _ _ _ -> Just (Shared.getModuleNameFromSummary modSummary)
    T_HscBackend _ _ mname _ _ _ -> Just (Plugins.moduleNameString mname)
    T_CmmCpp _ _ outputFilepath -> Just outputFilepath
    T_Cmm _ _ outputFilepath -> Just outputFilepath
    T_Cc _ _ _ _ outputFilepath -> Just outputFilepath
    T_As _ _ _ _ _ -> (Nothing)
    T_LlvmOpt _ _ _ -> (Nothing)
    T_LlvmLlc _ _ _ -> (Nothing)
    T_LlvmMangle _ _ _ -> (Nothing)
    T_MergeForeign _ _ objectFilePath _ -> Just objectFilePath
    _ -> Nothing
