{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Qi.Config.Identifier where

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Hashable
import           Protolude


class ParentResource a where
  toParentId :: a -> Either ApiId ApiResourceId

class FromInt a where
  fromInt :: Int -> a

newtype S3BucketId = S3BucketId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt S3BucketId where
  fromInt = S3BucketId

newtype LambdaId = LambdaId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt LambdaId where
  fromInt = LambdaId

newtype ApiId = ApiId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance ParentResource ApiId where
  toParentId = Left
instance FromInt ApiId where
  fromInt = ApiId

newtype ApiAuthorizerId = ApiAuthorizerId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt ApiAuthorizerId where
  fromInt = ApiAuthorizerId

newtype ApiResourceId = ApiResourceId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance ParentResource ApiResourceId where
  toParentId = Right
instance FromInt ApiResourceId where
  fromInt = ApiResourceId

newtype DdbTableId = DdbTableId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt DdbTableId where
  fromInt = DdbTableId

newtype SqsQueueId = SqsQueueId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt SqsQueueId where
  fromInt = SqsQueueId

newtype CustomId = CustomId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt CustomId where
  fromInt = CustomId

newtype CwEventsRuleId = CwEventsRuleId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt CwEventsRuleId where
  fromInt = CwEventsRuleId
