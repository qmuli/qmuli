{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qi.Config.Identifier where

import           Data.Hashable
import           System.Random

class ParentResource a where
  toParentId :: a -> Either ApiId ApiResourceId

newtype S3BucketId = S3BucketId Int deriving (Eq, Show, Random, Hashable)

newtype LambdaId = LambdaId Int deriving (Eq, Show, Random, Hashable)

newtype ApiId = ApiId Int deriving (Eq, Show, Random, Hashable)
instance ParentResource ApiId where
  toParentId = Left

newtype ApiAuthorizerId = ApiAuthorizerId Int deriving (Eq, Show, Random, Hashable)

newtype ApiResourceId = ApiResourceId Int deriving (Eq, Show, Random, Hashable)
instance ParentResource ApiResourceId where
  toParentId = Right

newtype DdbTableId = DdbTableId Int deriving (Eq, Show, Random, Hashable)

newtype CustomId = CustomId Int deriving (Eq, Show, Random, Hashable)
