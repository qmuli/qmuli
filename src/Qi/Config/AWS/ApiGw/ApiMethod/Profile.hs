{-# LANGUAGE TemplateHaskell #-}

module Qi.Config.AWS.ApiGw.ApiMethod.Profile where

import           Control.Lens
import           Data.Default         (Default, def)

import           Qi.Config.Identifier (ApiAuthorizerId)


data ApiMethodProfile = ApiMethodProfile {
    _ampAuthId :: Maybe ApiAuthorizerId
  }

instance Default ApiMethodProfile where
  def = ApiMethodProfile {
      _ampAuthId = Nothing
    }

makeLenses ''ApiMethodProfile

