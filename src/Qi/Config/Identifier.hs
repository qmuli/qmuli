{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Qi.Config.Identifier where

import           Data.Hashable


class ParentResource a where
  toParentId :: a -> Either ApiId ApiResourceId

class FromInt a where
  fromInt :: Int -> a

newtype S3BucketId = S3BucketId Int deriving (Eq, Show, Hashable)
instance FromInt S3BucketId where
  fromInt = S3BucketId

newtype LambdaId = LambdaId Int deriving (Eq, Show, Hashable)
instance FromInt LambdaId where
  fromInt = LambdaId

newtype ApiId = ApiId Int deriving (Eq, Show, Hashable)
instance ParentResource ApiId where
  toParentId = Left
instance FromInt ApiId where
  fromInt = ApiId

newtype ApiAuthorizerId = ApiAuthorizerId Int deriving (Eq, Show, Hashable)
instance FromInt ApiAuthorizerId where
  fromInt = ApiAuthorizerId

newtype ApiResourceId = ApiResourceId Int deriving (Eq, Show, Hashable)
instance ParentResource ApiResourceId where
  toParentId = Right
instance FromInt ApiResourceId where
  fromInt = ApiResourceId

newtype DdbTableId = DdbTableId Int deriving (Eq, Show, Hashable)
instance FromInt DdbTableId where
  fromInt = DdbTableId

newtype CustomId = CustomId Int deriving (Eq, Show, Hashable)
instance FromInt CustomId where
  fromInt = CustomId

newtype CwEventsRuleId = CwEventsRuleId Int deriving (Eq, Show, Hashable)
instance FromInt CwEventsRuleId where
  fromInt = CwEventsRuleId
