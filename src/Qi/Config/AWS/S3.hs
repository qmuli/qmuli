{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.S3 where

import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Stratosphere

import           Qi.Config.Identifier


data S3EventType =
    S3ObjectCreatedAll
  | S3ObjectRemovedAll
  deriving (Eq)

instance Show S3EventType where
  show S3ObjectCreatedAll = "s3:ObjectCreated:*"
  show S3ObjectRemovedAll = "s3:ObjectRemoved:*"


data LambdaEventConfig = LambdaEventConfig {
    _event :: S3EventType
  , _lbdId :: LambdaIdentifier
  } deriving Show

makeLenses ''LambdaEventConfig


newtype S3Key = S3Key Text
  deriving Show

data S3Object = S3Object {
    bucketId :: S3BucketIdentifier
  , key      :: S3Key
  } deriving Show

data S3Event = S3Event {
    s3Object :: S3Object
  } deriving Show


data S3Bucket = S3Bucket {
    _bucketName      :: Text
  , _lbdEventConfigs :: [LambdaEventConfig]
  } deriving Show

instance Hashable S3Bucket where
  hashWithSalt s S3Bucket{_bucketName} = s `hashWithSalt` _bucketName


makeLenses ''S3Bucket



data S3Config = S3Config {
    _s3Buckets :: HashMap S3BucketIdentifier S3Bucket
  } deriving Show

instance Monoid S3Config where
  S3Config { _s3Buckets = b1 } `mappend` S3Config { _s3Buckets = b2 } =
    S3Config { _s3Buckets = b1 `mappend` b2 }
  mempty = def

instance Default S3Config where
  def = S3Config {
    _s3Buckets = SHM.empty
  }


makeLenses ''S3Config




