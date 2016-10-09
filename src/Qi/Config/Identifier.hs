{-# LANGUAGE NamedFieldPuns #-}

module Qi.Config.Identifier where

import           Data.Hashable


newtype S3BucketIdentifier = S3BucketIdentifier Int deriving (Eq, Show)
instance Hashable S3BucketIdentifier where
  s `hashWithSalt` (S3BucketIdentifier identifierHash) = s `hashWithSalt` identifierHash

newtype LambdaIdentifier = LambdaIdentifier Int deriving (Eq, Show)
instance Hashable LambdaIdentifier where
  s `hashWithSalt` (LambdaIdentifier identifierHash) = s `hashWithSalt` identifierHash

