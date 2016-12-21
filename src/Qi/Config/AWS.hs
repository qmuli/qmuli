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

import           Qi.Config.AWS.ApiGw       (ApiGwConfig)
import           Qi.Config.AWS.CF          (CfConfig)
import           Qi.Config.AWS.DDB         (DdbConfig)
import           Qi.Config.AWS.Lambda      (LambdaConfig)
import           Qi.Config.AWS.S3          (S3Config)
import           Qi.Config.Identifier      (FromInt (..))


data Config = Config {
    _namePrefix  :: Text
  , _nextId      :: Int
  , _s3Config    :: S3Config
  , _apiGwConfig :: ApiGwConfig
  , _lbdConfig   :: LambdaConfig
  , _ddbConfig   :: DdbConfig
  , _cfConfig    :: CfConfig
}

instance Default Config where
  def = Config {
      _namePrefix = "qmuli"
    , _nextId     = 0  -- global autoincrement id state
    , _s3Config   = def
    , _apiGwConfig  = def
    , _lbdConfig  = def
    , _ddbConfig  = def
    , _cfConfig   = def
  }

makeLenses ''Config

-- get a resource-specific identifier based on the next autoincremented numeric id
-- while keeping the autoincrement state in the global Config
getNextId
  :: (MonadState Config m, FromInt a)
  => m a
getNextId = do
  nid <- use nextId
  nextId += 1
  return $ fromInt nid

underscoreNamePrefixWith :: Text -> Config -> Text
underscoreNamePrefixWith = namePrefixWith "_"

dotNamePrefixWith :: Text -> Config -> Text
dotNamePrefixWith = namePrefixWith "."

namePrefixWith
  :: Text
  -> Text
  -> Config
  -> Text
namePrefixWith sep name config = T.concat [config^.namePrefix, sep, name]

