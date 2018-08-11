{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.Config.Lang where

import           Control.Monad.Freer
import           Control.Monad.State.Strict            (State)
import           Data.Aeson                            (FromJSON, ToJSON)
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Lazy.Char8            as LBS
import           Data.Default                          (Default, def)
import           Protolude
import           Qi.Config.AWS                         (Config)
import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ApiMethodProfile)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda                  (LambdaProfile)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Core.Curry
import           Qi.Program.Lambda.Interface           (ApiLambdaProgram, CfCustomResourceLambdaProgram,
                                                        CwLambdaProgram,
                                                        DdbStreamLambdaProgram,
                                                        LambdaProgram,
                                                        S3LambdaProgram)




data ResEff r where
  RGenericLambda
    :: forall a b
    .  (FromJSON a, ToJSON b)
    => Text
    -> (a -> LambdaProgram b)
    -> LambdaProfile
    -> ResEff LambdaId

-- S3

  RS3Bucket
    :: Text
    -> ResEff S3BucketId

  RS3BucketLambda
    :: Text
    -> S3BucketId
    -> S3LambdaProgram
    -> LambdaProfile
    -> ResEff LambdaId

{-

-- DDB
  RDdbTable
    :: Text
    -> DdbAttrDef
    -> DdbTableProfile
    -> ResEff DdbTableId

  RDdbStreamLambda
    :: Text
    -> DdbTableId
    -> DdbStreamLambdaProgram
    -> LambdaProfile
    -> ResEff LambdaId

-- SQS
  RSqsQueue
    :: Text
    -> ResEff SqsQueueId

-- Api
  RApi
    :: Text
    -> ResEff ApiId

  RApiAuthorizer
    :: Text
    -> CfCustomResourceId
    -> ApiId
    -> ResEff ApiAuthorizerId

  RApiResource
    :: ParentResource a
    => Text
    -> a
    -> ResEff ApiResourceId

  RApiMethodLambda
    :: Text
    -> ApiVerb
    -> ApiResourceId
    -> ApiMethodProfile
    -> ApiLambdaProgram
    -> LambdaProfile
    -> ResEff LambdaId

-- Custom
  RCustomResource
    :: Text
    -> CfCustomResourceLambdaProgram
    -> LambdaProfile
    -> ResEff CfCustomResourceId

-- CloudWatch Logs
  RCwEventLambda
    :: Text
    -> CwEventsRuleProfile
    -> CwLambdaProgram
    -> LambdaProfile
    -> ResEff LambdaId

-}




genericLambda
  :: (Member ResEff effs, FromJSON a, ToJSON b)
  => Text
  -> (a -> LambdaProgram b)
  -> LambdaProfile
  -> Eff effs LambdaId
genericLambda = send .:: RGenericLambda

s3Bucket
  :: (Member ResEff effs)
  => Text
  -> Eff effs S3BucketId
s3Bucket = send . RS3Bucket

s3BucketLambda
  :: (Member ResEff effs)
  => Text
  -> S3BucketId
  -> S3LambdaProgram
  -> LambdaProfile
  -> Eff effs LambdaId
s3BucketLambda =
  send .::: RS3BucketLambda

{-

ddbTable
  :: Text
  -> DdbAttrDef
  -> DdbTableProfile
  -> ConfigProgram DdbTableId
ddbTable = singleton .:: RDdbTable

ddbStreamLambda
  :: Text
  -> DdbTableId
  -> DdbStreamLambdaProgram
  -> LambdaProfile
  -> ConfigProgram LambdaId
ddbStreamLambda = singleton .::: RDdbStreamLambda

sqsQueue
  :: Text
  -> ConfigProgram SqsQueueId
sqsQueue = singleton . RSqsQueue

api
  :: Text
  -> ConfigProgram ApiId
api = singleton . RApi

apiAuthorizer
  :: Text
  -> CfCustomResourceId
  -> ApiId
  -> ConfigProgram ApiAuthorizerId
apiAuthorizer =
  singleton .:: RApiAuthorizer

apiResource
  :: ParentResource a
  => Text
  -> a
  -> ConfigProgram ApiResourceId
apiResource =
  singleton .: RApiResource

apiMethodLambda
  :: Text
  -> ApiVerb
  -> ApiResourceId
  -> ApiMethodProfile
  -> ApiLambdaProgram
  -> LambdaProfile
  -> ConfigProgram LambdaId
apiMethodLambda =
  singleton .::::: RApiMethodLambda

customResource
  :: Text
  -> CfCustomResourceLambdaProgram
  -> LambdaProfile
  -> ConfigProgram CfCustomResourceId
customResource =
  singleton .:: RCustomResource

cwEventLambda
  :: Text
  -> CwEventsRuleProfile
  -> CwLambdaProgram
  -> LambdaProfile
  -> ConfigProgram LambdaId
cwEventLambda =
  singleton .::: RCwEventLambda
-}
