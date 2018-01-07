{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens                         hiding (argument)
import           Control.Monad.State.Strict           (runState)
import           Data.Char                            (isDigit, isLower)
import           Data.Default                         (def)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Protolude                            hiding (runState)
import           Qi.Config.AWS
import           Qi.Dispatcher
import           Qi.Options
import           Qi.Program.Config.Interface          (ConfigProgram)
import           Qi.Program.Config.Interpreters.Build (QiConfig (unQiConfig),
                                                       interpret)


withConfig
  :: ConfigProgram ()
  -> IO ()
withConfig configProgram = do
  args <- getArgs

  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  Options{ appName, cmd } <- showHelpOnErrorExecParser optionsSpec

  (`runReaderT` config appName) $ do

    case cmd of
      Cf CfTemplate                    -> renderCfTemplate
      Cf CfDeploy                      -> deployApp
      Cf CfCreate                      -> createCfStack
      Cf CfUpdate                      -> updateCfStack
      Cf CfDescribe                    -> describeCfStack
      Cf CfDestroy                     -> destroyCfStack $ return ()
      Cf CfCycle                       -> cycleStack

      Lbd LbdUpdate                    -> updateLambdas
      Lbd (LbdSendEvent lbdName event) -> invokeLambda lbdName event

  where
    config name = snd . (`runState` def{_namePrefix = name}) . unQiConfig $ interpret configProgram



