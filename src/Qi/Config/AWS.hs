{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Qi.Config.AWS where

import           Control.Lens
import           Control.Monad.State.Class (MonadState)
import           Data.Default              (Default, def)
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Qi.Config.AWS.Api         (ApiConfig)
import           Qi.Config.AWS.CF          (CfConfig)
import           Qi.Config.AWS.DDB         (DdbConfig)
import           Qi.Config.AWS.Lambda      (LambdaConfig)
import           Qi.Config.AWS.S3          (S3Config)
import           Qi.Config.Identifier      (FromInt (..))


data Config = Config {
    _namePrefix :: Text
  , _nextId     :: Int
  , _s3Config   :: S3Config
  , _apiConfig  :: ApiConfig
  , _lbdConfig  :: LambdaConfig
  , _ddbConfig  :: DdbConfig
  , _cfConfig   :: CfConfig
} deriving Show

instance Default Config where
  def = Config {
      _namePrefix = "qmuli"
    , _nextId     = 0
    , _s3Config   = def
    , _apiConfig  = def
    , _lbdConfig  = def
    , _ddbConfig  = def
    , _cfConfig   = def
  }

makeLenses ''Config

getNextId
  :: (MonadState Config m, FromInt a)
  => m a
getNextId = do
  nid <- use nextId
  nextId += 1
  return $ fromInt nid

underscoreNamePrefixWith = namePrefixWith "_"
dotNamePrefixWith = namePrefixWith "."

namePrefixWith
  :: Text
  -> Text
  -> Config
  -> Text
namePrefixWith sep name config = T.concat [config ^. namePrefix, sep, name]

