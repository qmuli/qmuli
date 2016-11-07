{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS where

import           Control.Lens
import           Data.Default         (Default, def)
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS.Api    (ApiConfig)
import           Qi.Config.AWS.Lambda (LambdaConfig)
import           Qi.Config.AWS.S3


data Config = Config {
    _namePrefix :: Text
  , _s3Config   :: S3Config
  , _apiConfig  :: ApiConfig
  , _lbdConfig  :: LambdaConfig
} deriving Show

instance Monoid Config where
  mappend
    Config {
        _namePrefix = np
      , _s3Config = s3c1
      , _apiConfig = api1
      , _lbdConfig = lc1
      }
    Config {
        _namePrefix = _
      , _s3Config = s3c2
      , _apiConfig = api2
      , _lbdConfig = lc2
      } =
    Config {
        _namePrefix = np
      , _s3Config = s3c1 `mappend` s3c2
      , _apiConfig = api1 `mappend` api2
      , _lbdConfig = lc1 `mappend` lc2
      }
  mempty = def

instance Default Config where
  def = Config {
      _namePrefix = "qmuli"
    , _s3Config = def
    , _apiConfig = def
    , _lbdConfig = def
  }

makeLenses ''Config

namePrefixWith :: Text -> Config -> Text
namePrefixWith name config = T.concat [config ^. namePrefix, "-", name]

