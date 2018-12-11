{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.S3 where

import           Control.Lens
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           GHC.Show             (Show (..))
import           Protolude
import           Qi.Config.Identifier
import           Qi.Config.Types


data S3Config = S3Config {
    _s3IdToBucket :: HashMap S3BucketId S3Bucket
  , _s3NameToId   :: HashMap Text S3BucketId
  }
  deriving (Eq, Show)
instance Default S3Config where
  def = S3Config {
      _s3IdToBucket     = SHM.empty
    , _s3NameToId = SHM.empty
    }

data S3Bucket = S3Bucket {
    _s3bName         :: Text
  , _s3bProfile      :: S3BucketProfile
  , _s3bEventConfigs :: [ S3EventConfig ]
  }
  deriving (Eq, Show)
instance Default S3Bucket where
  def = S3Bucket {
      _s3bName         = "default"
    , _s3bProfile      = def
    , _s3bEventConfigs = def
    }

data S3BucketProfile = S3BucketProfile {
    _s3bpExistence    :: ResourceExistence
  }
  deriving (Eq, Show)
instance Default S3BucketProfile where
  def = S3BucketProfile {
      _s3bpExistence = ShouldCreate
    }

data S3EventType =
    S3ObjectCreatedAll
  | S3ObjectRemovedAll
  deriving Eq
instance Show S3EventType where
  show S3ObjectCreatedAll = "s3:ObjectCreated:*"
  show S3ObjectRemovedAll = "s3:ObjectRemoved:*"

newtype S3Key = S3Key Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data S3Object = S3Object {
    _s3oBucketId :: S3BucketId
  , _s3oKey      :: S3Key
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

s3Object
  :: S3BucketId
  -> S3Key
  -> S3Object
s3Object = S3Object

data S3Event = S3Event {
    _s3eObject :: S3Object
  }
  deriving (Eq, Show)


data S3EventConfig = S3EventConfig {
    _event :: S3EventType
  , _lbdId :: LambdaId
  }
  deriving (Eq, Show)


makeLenses ''S3EventConfig
makeLenses ''S3Object
makeLenses ''S3Bucket
makeLenses ''S3BucketProfile
makeLenses ''S3Event
makeLenses ''S3Config



