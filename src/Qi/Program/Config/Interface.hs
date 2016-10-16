{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Config.Interface where

import           Control.Monad.Operational   (Program, singleton)
import           Control.Monad.State.Strict  (State)
import           Data.ByteString             (ByteString)
import           Data.Default                (Default, def)
import           Data.Text                   (Text)

import           Qi.Config.AWS               (Config)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (LambdaProgram)


type ConfigProgram a = Program ConfigInstruction a

data ConfigInstruction a where
  CreateS3Bucket
    :: Text
    -> ConfigInstruction S3BucketIdentifier
  CreateS3BucketLambda
    :: Text
    -> S3BucketIdentifier
    -> (S3Event -> LambdaProgram ())
    -> ConfigInstruction LambdaIdentifier


createS3Bucket = singleton . CreateS3Bucket

createS3BucketLambda name s3Id = singleton . CreateS3BucketLambda name s3Id


