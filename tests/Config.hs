{-# LANGUAGE OverloadedStrings #-}

module Config (
    getTemplate
  , getOutputs
  , getConfig
  , appName) where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import           Data.Aeson                           (Value, decode)
import           Data.Aeson.Lens                      (key)
import           Data.Default                         (def)
import           Data.Maybe                           (fromJust, isJust)

import           Qi.Config.AWS                        (Config (..))
import           Qi.Config.CF                         (render)
import           Qi.Program.Config.Interpreters.Build (interpret, unQiConfig)

import           Util


appName = "testName"

getOutputs :: Value -> Value
getOutputs t = fromJust $ t ^? key "Outputs"

getTemplate :: Config -> Value
getTemplate cfg = fromJust (decode $ render cfg)

getConfig cp = snd
  . (`runState` def{_namePrefix = appName})
  . unQiConfig
  $ interpret cp
