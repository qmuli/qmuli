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
import           Qi.Config.AWS.Api
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (LambdaProgram)


type ConfigProgram a = Program ConfigInstruction a

data ConfigInstruction a where
  CreateS3Bucket
    :: Text
    -> ConfigInstruction S3BucketId

  CreateS3BucketLambda
    :: Text
    -> S3BucketId
    -> (S3Event -> LambdaProgram ())
    -> ConfigInstruction LambdaId

  CreateApi
    :: Text
    -> ConfigInstruction ApiId

  CreateApiRootResource
    :: Text
    -> ApiId
    -> ConfigInstruction ApiResourceId

  CreateApiChildResource
    :: Text
    -> ApiResourceId
    -> ConfigInstruction ApiResourceId

  CreateApiMethodLambda
    :: Text
    -> ApiVerb
    -> ApiResourceId
    -> (ApiEvent -> LambdaProgram ())
    -> ConfigInstruction LambdaId


createS3Bucket = singleton . CreateS3Bucket

createS3BucketLambda name s3BucketId = singleton . CreateS3BucketLambda name s3BucketId

createApi = singleton . CreateApi

createApiRootResource name = singleton . CreateApiRootResource name

createApiChildResource name = singleton . CreateApiChildResource name

createApiMethodLambda name verb resourceId = singleton . CreateApiMethodLambda name verb resourceId

