{-# LANGUAGE NamedFieldPuns #-}

module Qi.Config.Identifier where

import           Data.Hashable

class ParentResource a where
  toParentId :: a -> Either ApiId ApiResourceId

newtype S3BucketId = S3BucketId Int deriving (Eq, Show)
instance Hashable S3BucketId where
  s `hashWithSalt` (S3BucketId identifierHash) = s `hashWithSalt` identifierHash

newtype LambdaId = LambdaId Int deriving (Eq, Show)
instance Hashable LambdaId where
  s `hashWithSalt` (LambdaId identifierHash) = s `hashWithSalt` identifierHash

newtype ApiId = ApiId Int deriving (Eq, Show)
instance Hashable ApiId where
  s `hashWithSalt` (ApiId identifierHash) = s `hashWithSalt` identifierHash
instance ParentResource ApiId where
  toParentId = Left

newtype ApiResourceId = ApiResourceId Int deriving (Eq, Show)
instance Hashable ApiResourceId where
  s `hashWithSalt` (ApiResourceId identifierHash) = s `hashWithSalt` identifierHash
instance ParentResource ApiResourceId where
  toParentId = Right

newtype DdbTableId = DdbTableId Int deriving (Eq, Show)
instance Hashable DdbTableId where
  s `hashWithSalt` (DdbTableId identifierHash) = s `hashWithSalt` identifierHash

newtype CustomId = CustomId Int deriving (Eq, Show)
instance Hashable CustomId where
  s `hashWithSalt` (CustomId identifierHash) = s `hashWithSalt` identifierHash
