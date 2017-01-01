{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi where

import           Control.Lens
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.State.Strict           (runState)
import           Control.Monad.Trans.Reader           (runReaderT)
import           Data.Char                            (isDigit, isLower)
import           Data.Default                         (def)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           System.Environment                   (getArgs, withArgs)

import           Qi.Config.AWS
import           Qi.Dispatcher
import           Qi.Program.Config.Interface          (ConfigProgram)
import           Qi.Program.Config.Interpreters.Build (QiConfig (unQiConfig),
                                                       interpret)


withConfig
  :: ConfigProgram ()
  -> IO ()
withConfig configProgram = do
  args <- getArgs
  case args of
    (appName:rest) ->
      if invalid appName
        then
          putStrLn $ "Invalid qmulus name: '" ++ appName ++ "', the name should only contain alphanumeric lower case characters or a hyphen"
        else
          withArgs rest $ withNameAndConfig (T.pack appName) configProgram
    _ -> putStrLn "Please provide a unique application name for your qmulus"

  where
    invalid = not . all (\c -> isLower c || isDigit c || c == '-')


withNameAndConfig
  :: Text
  -> ConfigProgram ()
  -> IO ()
withNameAndConfig appName configProgram = do
  args <- getArgs
  (flip runReaderT) config $ do
    case args of
      "cf":"template":[]  -> renderCfTemplate
      "cf":"deploy":[]    -> deployApp
      "cf":"create":[]    -> createCfStack
      "cf":"update":[]    -> updateCfStack
      "cf":"describe":[]  -> describeCfStack
      "cf":"destroy":[]   -> destroyCfStack $ return ()
      "cf":"cycle":[]     -> cycleStack

      "lbd":lbdName:event:[] -> invokeLambda lbdName event

      _ -> liftIO . putStrLn $ "Unexpected arguments: '" ++ show args ++ "'"

  where
    config = snd . (`runState` def{_namePrefix = appName}) . unQiConfig $ interpret configProgram



