{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens                         hiding (argument)
import           Control.Monad.State.Strict           (runState)
import           Data.Char                            (isDigit, isLower)
import           Data.Default                         (def)
import           Protolude                            hiding (runState)
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
import           Qi.Options
import           Qi.Program.Config.Interface          (ConfigProgram)
import           Qi.Program.Config.Interpreters.Build (QiConfig (unQiConfig),
                                                       interpret)


withConfig
  :: ConfigProgram ()
  -> IO ()
withConfig configProgram = do
  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  Options{ appName, cmd } <- showHelpOnErrorExecParser optionsSpec

  (`runReaderT` config appName) $ do

    case cmd of
      CfRenderTemplate           -> renderCfTemplate
      CfDeploy                   -> deployApp
      CfCreate                   -> createCfStack
      CfUpdate                   -> updateCfStack
      CfDescribe                 -> describeCfStack
      CfDestroy                  -> destroyCfStack $ pure ()
      CfCycle                    -> cycleStack

      LbdUpdate                  -> updateLambdas
      LbdSendEvent lbdName event -> invokeLambda lbdName event
      LbdLogs lbdName            -> lambdaLogs lbdName

  where
    config name = snd . (`runState` def{_namePrefix = name}) . unQiConfig $ interpret configProgram



