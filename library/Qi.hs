{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens                  hiding (argument)
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Char                     (isDigit, isLower)
import           Data.Default                  (def)
import           Protolude                     hiding (State, runState)
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
import           Qi.Options
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang        (ConfigEff)


withConfig
  :: Eff '[ConfigEff, State Config] ()
  -> IO ()
withConfig configProgram = do
  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  Options{ appName, cmd } <- showHelpOnErrorExecParser optionsSpec

  (`runReaderT` config appName) $ do

    case cmd of
      CfRenderTemplate     -> renderCfTemplate
      CfDeploy             -> deployApp
      CfCreate             -> createCfStack
      CfUpdate             -> updateCfStack
      CfDescribe           -> describeCfStack
      CfDestroy            -> destroyCfStack $ pure ()
      CfCycle              -> cycleStack

      LbdUpdate            -> updateLambdas
      LbdSendEvent lbdName -> invokeLambda lbdName
      LbdLogs lbdName      -> lambdaLogs lbdName

  where
    config name =
        snd
      . run
      . runState def{_namePrefix = name}
      . Config.run
      $ configProgram


