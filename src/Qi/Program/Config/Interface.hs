{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Config.Interface where

import           Control.Monad.Operational             (Program, singleton)
import           Control.Monad.State.Strict            (State)
import           Data.ByteString                       (ByteString)
import           Data.Default                          (Default, def)
import           Data.Text                             (Text)

import           Qi.Config.AWS                         (Config)
import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ApiMethodProfile)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda                  (LambdaProfile)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface           (LambdaProgram)


type ConfigProgram a = Program ConfigInstruction a

data ConfigInstruction a where
  RS3Bucket
    :: Text
    -> ConfigInstruction S3BucketId

  RS3BucketLambda
    :: Text
    -> S3BucketId
    -> (S3Event -> LambdaProgram ())
    -> LambdaProfile
    -> ConfigInstruction LambdaId

  RDdbTable
    :: Text
    -> DdbAttrDef
    -> DdbTableProfile
    -> ConfigInstruction DdbTableId

  RApi
    :: Text
    -> ConfigInstruction ApiId

  RApiAuthorizer
    :: Text
    -> CustomId
    -> ApiId
    -> ConfigInstruction ApiAuthorizerId

  RApiResource
    :: ParentResource a
    => Text
    -> a
    -> ConfigInstruction ApiResourceId

  RApiMethodLambda
    :: Text
    -> ApiVerb
    -> ApiResourceId
    -> ApiMethodProfile
    -> (ApiMethodEvent -> LambdaProgram ())
    -> LambdaProfile
    -> ConfigInstruction LambdaId

  RCustomResource
    :: Text
    -> (CfEvent -> LambdaProgram ())
    -> LambdaProfile
    -> ConfigInstruction CustomId


s3Bucket = singleton . RS3Bucket

s3BucketLambda name s3BucketId lbd = singleton . RS3BucketLambda name s3BucketId lbd

ddbTable name hashAttrDef = singleton . RDdbTable name hashAttrDef

api = singleton . RApi

apiAuthorizer name cognitoId =
  singleton . RApiAuthorizer name cognitoId

apiResource
  :: ParentResource a
  => Text
  -> a
  -> ConfigProgram ApiResourceId
apiResource name = singleton . RApiResource name

apiMethodLambda name verb resourceId methodProfile lbd =
  singleton . RApiMethodLambda name verb resourceId methodProfile lbd

customResource name lbd = singleton . RCustomResource name lbd

