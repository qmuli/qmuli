{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS where

import           Control.Lens
import           Data.Default         (Default, def)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS.Api    (ApiConfig)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda (LambdaConfig)
import           Qi.Config.AWS.S3


data Config = Config {
    _namePrefix :: Text
  , _s3Config   :: S3Config
  , _apiConfig  :: ApiConfig
  , _lbdConfig  :: LambdaConfig
  , _ddbConfig  :: DdbConfig
  , _cfConfig   :: CfConfig
} deriving Show

instance Default Config where
  def = Config {
      _namePrefix = "qmuli"
    , _s3Config = def
    , _apiConfig = def
    , _lbdConfig = def
    , _ddbConfig = def
    , _cfConfig = def
  }

makeLenses ''Config

underscoreNamePrefixWith = namePrefixWith "_"
dotNamePrefixWith = namePrefixWith "."

namePrefixWith
  :: Text
  -> Text
  -> Config
  -> Text
namePrefixWith sep name config = T.concat [config ^. namePrefix, sep, name]

