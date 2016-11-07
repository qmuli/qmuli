{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Config.AWS.Api.Event where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Api
import           Qi.Config.AWS.Api.Accessors


parse
  :: Value
  -> Config
  -> Parser ApiEvent
parse (Object e) config = do
    body <- e .: "body"
    return $ ApiEvent body
parse v _ =
    fail "event must be a json object and contain \"body\""
