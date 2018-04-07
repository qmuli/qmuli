{-# LANGUAGE OverloadedStrings #-}

module Config (
    getTemplate
  , getResources
  , getOutputs
  , getConfig
  , appName) where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import           Data.Aeson                           (Value, decode)
import           Data.Aeson.Lens                      (key)
import           Data.Default                         (def)
import           Data.Maybe                           (fromJust, isJust)
import           Data.Text                            (Text)
import           Protolude                            hiding (runState)
import           Qi.Config.AWS                        (Config (..))
import           Qi.Config.CF                         (render)
import           Qi.Program.Config.Interface          (ConfigProgram)
import           Qi.Program.Config.Interpreters.Build (interpret, unQiConfig)
import           Util


appName :: Text
appName = "testApp"

getResources :: Value -> Value
getResources = getValueUnderKey "Resources"

getOutputs :: Value -> Value
getOutputs = getValueUnderKey "Outputs"

getTemplate :: Config -> Value
getTemplate cfg = fromJust (decode $ render cfg)

getConfig :: ConfigProgram () -> Config
getConfig cp = snd
  . (`runState` def{_namePrefix = appName})
  . unQiConfig
  $ interpret cp




