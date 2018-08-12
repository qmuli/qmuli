{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Qi.Program.Config.Lang where

import           Control.Monad.Freer
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default               (Default, def)
import           Protolude
import           Qi.Config.AWS              (Config)
{- import           Qi.Config.AWS.ApiGw -}
{- import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ApiMethodProfile) -}
{- import           Qi.Config.AWS.CF -}
{- import           Qi.Config.AWS.CW -}
{- import           Qi.Config.AWS.DDB -}
import           Qi.Config.AWS.Lambda       (LambdaProfile)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Core.Curry
{- import           Qi.Program.Lambda.Cf.Lang  (CfCustomResourceLambdaProgram) -}
{- import           Qi.Program.Lambda.Cw.Lang  (CwLambdaProgram) -}
{- import           Qi.Program.Lambda.Ddb.Lang (DdbStreamLambdaProgram) -}
import           Qi.Program.Gen.Lang
{- import           Qi.Program.Lambda.Lang     (LbdEff) -}
import           Qi.Program.S3.Lang




data ResEff r where
  RGenericLambda
    :: forall a b
    .  (FromJSON a, ToJSON b)
    => Proxy a
    -> Proxy b
    -> Text
    -> (forall effs . (Member GenEff effs, Member S3Eff effs) => a -> Eff effs b)
    -> LambdaProfile
    -> ResEff LambdaId

-- S3
  RS3Bucket
    :: Text
    -> ResEff S3BucketId

  RS3BucketLambda
    :: Text
    -> S3BucketId
    -> (forall effs . (Member GenEff effs, Member S3Eff effs) => S3LambdaProgram effs)
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
    -> (forall effs . Member DdbEff effs => a -> effs b)
    -> LambdaProfile
    -> ResEff LambdaId

-- SQS
  RSqsQueue
    :: Text
    -> ResEff SqsQueueId


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


{-
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
-}


genericLambda
  :: forall a b resEffs
  .  (Member ResEff resEffs, FromJSON a, ToJSON b)
  => Text
  -> (forall effs . (Member GenEff effs, Member S3Eff effs) => a -> Eff effs b)
  -> LambdaProfile
  -> Eff resEffs LambdaId
genericLambda name f =
  send . RGenericLambda (Proxy :: Proxy a) (Proxy :: Proxy b) name f

s3Bucket
  :: (Member ResEff effs)
  => Text
  -> Eff effs S3BucketId
s3Bucket =
  send . RS3Bucket

s3BucketLambda
  :: forall resEffs
  .  (Member ResEff resEffs)
  => Text
  -> S3BucketId
  -> (forall effs . (Member GenEff effs, Member S3Eff effs) => S3LambdaProgram effs)
  -> LambdaProfile
  -> Eff resEffs LambdaId
s3BucketLambda name bucketId f =
  send . RS3BucketLambda name bucketId f

{-
ddbTable
  :: (Member ResEff effs)
  => Text
  -> DdbAttrDef
  -> DdbTableProfile
  -> Eff effs DdbTableId
ddbTable =
  send .:: RDdbTable

ddbStreamLambda
  :: (Member ResEff effs)
  => Text
  -> DdbTableId
  -> DdbStreamLambdaProgram
  -> LambdaProfile
  -> Eff effs LambdaId
ddbStreamLambda =
  send .::: RDdbStreamLambda

sqsQueue
  :: (Member ResEff effs)
  => Text
  -> Eff effs SqsQueueId
sqsQueue =
  send . RSqsQueue


customResource
  :: (Member ResEff effs)
  => Text
  -> CfCustomResourceLambdaProgram
  -> LambdaProfile
  -> Eff effs CfCustomResourceId
customResource =
  send .:: RCustomResource

cwEventLambda
  :: (Member ResEff effs)
  => Text
  -> CwEventsRuleProfile
  -> CwLambdaProgram
  -> LambdaProfile
  -> Eff effs LambdaId
cwEventLambda =
  send .::: RCwEventLambda
-}

{-

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
-}

