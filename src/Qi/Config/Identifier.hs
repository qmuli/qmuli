{-# LANGUAGE NamedFieldPuns #-}

module Qi.Config.Identifier where

import           Data.Hashable


newtype S3BucketId = S3BucketId Int deriving (Eq, Show)
instance Hashable S3BucketId where
  s `hashWithSalt` (S3BucketId identifierHash) = s `hashWithSalt` identifierHash

newtype LambdaId = LambdaId Int deriving (Eq, Show)
instance Hashable LambdaId where
  s `hashWithSalt` (LambdaId identifierHash) = s `hashWithSalt` identifierHash

newtype ApiId = ApiId Int deriving (Eq, Show)
instance Hashable ApiId where
  s `hashWithSalt` (ApiId identifierHash) = s `hashWithSalt` identifierHash

newtype ApiResourceId = ApiResourceId Int deriving (Eq, Show)
instance Hashable ApiResourceId where
  s `hashWithSalt` (ApiResourceId identifierHash) = s `hashWithSalt` identifierHash

newtype ApiMethodId = ApiMethodId Int deriving (Eq, Show)
instance Hashable ApiMethodId where
  s `hashWithSalt` (ApiMethodId identifierHash) = s `hashWithSalt` identifierHash

