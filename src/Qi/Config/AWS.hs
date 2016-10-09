{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS where

import           Control.Lens
import           Data.Default         (Default, def)
import           Qi.Config.AWS.Lambda (Lambda, LambdaConfig, lcLambdas)
import           Qi.Config.AWS.S3     (S3Config)


data Config = Config {
    _s3Config  :: S3Config
  , _lbdConfig :: LambdaConfig
} deriving Show

instance Monoid Config where
  mappend
    Config {
      _s3Config = s3c1
    , _lbdConfig = lc1
      }
    Config {
        _s3Config = s3c2
      , _lbdConfig = lc2
      } =
    Config {
        _s3Config = s3c1 `mappend` s3c2
      , _lbdConfig = lc1 `mappend` lc2
      }
  mempty = def

instance Default Config where
  def = Config {
      _s3Config = def
    , _lbdConfig = def
  }

makeLenses ''Config


