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
    s3oBucketId :: S3BucketIdentifier
  , s3oKey      :: S3Key
  } deriving Show


data S3Event = S3Event {
    s3Object :: S3Object
  } deriving Show



data S3Bucket = S3Bucket {
    _s3bName            :: Text
  , _s3bLbdEventConfigs :: [LambdaEventConfig]
  } deriving Show

instance Hashable S3Bucket where
  hashWithSalt s S3Bucket{_s3bName} = s `hashWithSalt` _s3bName


makeLenses ''S3Bucket



data S3BucketIndex = S3BucketIndex {
    _s3idxIdToS3Bucket :: HashMap S3BucketIdentifier S3Bucket
  , _s3idxNameToId     :: HashMap Text S3BucketIdentifier
  } deriving Show
makeLenses ''S3BucketIndex

instance Monoid S3BucketIndex where
  mappend
    S3BucketIndex { _s3idxIdToS3Bucket = idxa, _s3idxNameToId = idxb }
    S3BucketIndex { _s3idxIdToS3Bucket = idxa2, _s3idxNameToId = idxb2 } =
      S3BucketIndex { _s3idxIdToS3Bucket = idxa `mappend` idxa2, _s3idxNameToId = idxb `mappend` idxb2 }
  mempty = def

instance Default S3BucketIndex where
  def = S3BucketIndex {
    _s3idxIdToS3Bucket  = SHM.empty
  , _s3idxNameToId      = SHM.empty
  }


data S3Config = S3Config {
    _s3Buckets :: S3BucketIndex
  } deriving Show

instance Monoid S3Config where
  S3Config { _s3Buckets = b1 } `mappend` S3Config { _s3Buckets = b2 } =
    S3Config { _s3Buckets = b1 `mappend` b2 }
  mempty = def

instance Default S3Config where
  def = S3Config {
    _s3Buckets = def
  }

makeLenses ''S3Config


insertBucket
  :: S3Bucket
  -> (S3BucketIdentifier, (S3Config -> S3Config))
insertBucket bucket = (bid, insertNameToId . insertIdToS3Bucket)
  where
    insertIdToS3Bucket :: S3Config -> S3Config
    insertIdToS3Bucket = over (s3Buckets . s3idxIdToS3Bucket) (SHM.insert bid bucket)

    insertNameToId :: S3Config -> S3Config
    insertNameToId = over (s3Buckets . s3idxNameToId) (SHM.insert bname bid)

    bid = S3BucketIdentifier $ hash bucket
    bname = bucket ^. s3bName


