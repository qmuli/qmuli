{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Config.Interface where

import           Control.Monad.Operational             (Program, ProgramT,
                                                        singleton)
import           Control.Monad.State.Strict            (State)
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
import           Qi.Program.Lambda.Interface           (ApiLambdaProgram,
                                                        CfLambdaProgram,
                                                        CwLambdaProgram,
                                                        DdbStreamLambdaProgram,
                                                        GenericLambdaProgram,
                                                        S3LambdaProgram)


type ConfigProgram a = Program ConfigInstruction a

data ConfigInstruction a where

-- Lambda
  RGenericLambda
    :: Text
    -> GenericLambdaProgram
    -> LambdaProfile
    -> ConfigInstruction LambdaId

-- S3
  RS3Bucket
    :: Text
    -> ConfigInstruction S3BucketId

  RS3BucketLambda
    :: Text
    -> S3BucketId
    -> S3LambdaProgram
    -> LambdaProfile
    -> ConfigInstruction LambdaId

-- DDB
  RDdbTable
    :: Text
    -> DdbAttrDef
    -> DdbTableProfile
    -> ConfigInstruction DdbTableId

  RDdbStreamLambda
    :: Text
    -> DdbTableId
    -> DdbStreamLambdaProgram
    -> LambdaProfile
    -> ConfigInstruction LambdaId

-- SQS
  RSqsQueue
    :: Text
    -> ConfigInstruction SqsQueueId

-- Api
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
    -> ApiLambdaProgram
    -> LambdaProfile
    -> ConfigInstruction LambdaId

-- Custom
  RCustomResource
    :: Text
    -> CfLambdaProgram
    -> LambdaProfile
    -> ConfigInstruction CustomId

-- CloudWatch Logs
  RCwEventLambda
    :: Text
    -> CwEventsRuleProfile
    -> CwLambdaProgram
    -> LambdaProfile
    -> ConfigInstruction LambdaId


genericLambda
  :: Text
  -> GenericLambdaProgram
  -> LambdaProfile
  -> ProgramT ConfigInstruction m LambdaId
genericLambda name lbd = singleton . RGenericLambda name lbd

s3Bucket
  :: Text
  -> ProgramT ConfigInstruction m S3BucketId
s3Bucket = singleton . RS3Bucket

s3BucketLambda :: Text
                  -> S3BucketId
                  -> S3LambdaProgram
                  -> LambdaProfile
                  -> ProgramT ConfigInstruction m LambdaId
s3BucketLambda name s3BucketId lbd =
  singleton . RS3BucketLambda name s3BucketId lbd

ddbTable
  :: Text
  -> DdbAttrDef
  -> DdbTableProfile
  -> ProgramT ConfigInstruction m DdbTableId
ddbTable name hashAttrDef = singleton . RDdbTable name hashAttrDef

ddbStreamLambda
  :: Text
  -> DdbTableId
  -> DdbStreamLambdaProgram
  -> LambdaProfile
  -> ProgramT ConfigInstruction m LambdaId
ddbStreamLambda name tableId lbd = singleton . RDdbStreamLambda name tableId lbd

sqsQueue
  :: Text
  -> ProgramT ConfigInstruction m SqsQueueId
sqsQueue = singleton . RSqsQueue

api
  :: Text
  -> ProgramT ConfigInstruction m ApiId
api = singleton . RApi

apiAuthorizer
  :: Text
  -> CustomId
  -> ApiId
  -> ProgramT ConfigInstruction m ApiAuthorizerId
apiAuthorizer name cognitoId =
  singleton . RApiAuthorizer name cognitoId


apiResource
  :: ParentResource a
  => Text
  -> a
  -> ConfigProgram ApiResourceId
apiResource name =
  singleton . RApiResource name

apiMethodLambda
  :: Text
  -> ApiVerb
  -> ApiResourceId
  -> ApiMethodProfile
  -> ApiLambdaProgram
  -> LambdaProfile
  -> ProgramT ConfigInstruction m LambdaId
apiMethodLambda name verb resourceId methodProfile lbd =
  singleton . RApiMethodLambda name verb resourceId methodProfile lbd

customResource
  :: Text
  -> CfLambdaProgram
  -> LambdaProfile
  -> ProgramT ConfigInstruction m CustomId
customResource name lbd =
  singleton . RCustomResource name lbd

cwEventLambda
  :: Text
  -> CwEventsRuleProfile
  -> CwLambdaProgram
  -> LambdaProfile
  -> ProgramT ConfigInstruction m LambdaId
cwEventLambda name ruleProfile lbdProgramFunc =
  singleton . RCwEventLambda name ruleProfile lbdProgramFunc

