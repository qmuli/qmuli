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
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (LambdaProgram)


type ConfigProgram a = Program ConfigInstruction a

data ConfigInstruction a where
  RS3Bucket
    :: Text
    -> ConfigInstruction S3BucketId

  RS3BucketLambda
    :: Text
    -> S3BucketId
    -> (S3Event -> LambdaProgram ())
    -> ConfigInstruction LambdaId

  RDdbTable
    :: Text
    -> DdbAttrDef
    -> Maybe DdbAttrDef
    -> DdbProvCap
    -> ConfigInstruction DdbTableId

  RApi
    :: Text
    -> ConfigInstruction ApiId

  RApiResource
    :: ParentResource a
    => Text
    -> a
    -> ConfigInstruction ApiResourceId

  RApiMethodLambda
    :: Text
    -> ApiVerb
    -> ApiResourceId
    -> (ApiEvent -> LambdaProgram ())
    -> ConfigInstruction LambdaId

  RCustomResourceLambda
    :: Text
    -> (CfEvent -> LambdaProgram ())
    -> ConfigInstruction LambdaId


s3Bucket = singleton . RS3Bucket

s3BucketLambda name s3BucketId = singleton . RS3BucketLambda name s3BucketId

ddbTable name hashAttrDef rangeAttrDef = singleton . RDdbTable name hashAttrDef rangeAttrDef

api = singleton . RApi

apiResource
  :: ParentResource a
  => Text
  -> a
  -> ConfigProgram ApiResourceId
apiResource name = singleton . RApiResource name

apiMethodLambda name verb resourceId = singleton . RApiMethodLambda name verb resourceId

customResourceLambda name = singleton . RCustomResourceLambda name

