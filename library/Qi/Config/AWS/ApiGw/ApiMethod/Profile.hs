{-# LANGUAGE TemplateHaskell #-}

module Qi.Config.AWS.ApiGw.ApiMethod.Profile where

import           Control.Lens
import           Data.Default         (Default, def)
import           Protolude

import           Qi.Config.Identifier (ApiAuthorizerId)


data ApiMethodProfile = ApiMethodProfile {
    _ampAuthId :: Maybe ApiAuthorizerId
  }
  deriving (Eq, Show)

instance Default ApiMethodProfile where
  def = ApiMethodProfile {
      _ampAuthId = Nothing
    }

makeLenses ''ApiMethodProfile

