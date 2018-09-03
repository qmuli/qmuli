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
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy.Char8     as LBS
import           Data.Default                   (Default, def)
import           Protolude
import           Qi.Config.AWS                  (Config)
{- import           Qi.Config.AWS.ApiGw -}
{- import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ApiMethodProfile) -}
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CW
{- import           Qi.Config.AWS.DDB -}
import           Qi.Config.AWS.CfCustomResource (CfCustomResourceLambdaProgram)
import           Qi.Config.AWS.Lambda           (LambdaProfile)
import           Qi.Config.Identifier
import           Qi.Core.Curry
{- import           Qi.Program.Lambda.Cf.Lang  (CfCustomResourceLambdaProgram) -}
{- import           Qi.Program.Lambda.Cw.Lang  (CwLambdaProgram) -}
{- import           Qi.Program.Lambda.Ddb.Lang (DdbStreamLambdaProgram) -}
import           Qi.Program.Gen.Lang
{- import           Qi.Program.Lambda.Lang     (LbdEff) -}
import           Qi.Program.S3.Lang


data ConfigEff r where

  GetConfig
    :: ConfigEff Config


  RegGenericLambda
    :: forall a b
    .  (FromJSON a, ToJSON b)
    => Proxy a
    -> Proxy b
    -> Text
    -> (forall effs . (Member GenEff effs, Member S3Eff effs) => a -> Eff effs b)
    -> LambdaProfile
    -> ConfigEff LambdaId

-- S3
  RegS3Bucket
    :: Text
    -> ConfigEff S3BucketId

  RegS3BucketLambda
    :: Text
    -> S3BucketId
    -> (forall effs . (Member GenEff effs, Member S3Eff effs) => S3LambdaProgram effs)
    -> LambdaProfile
    -> ConfigEff LambdaId
{-
-- DDB
  RDdbTable
    :: Text
    -> DdbAttrDef
    -> DdbTableProfile
    -> ConfigEff DdbTableId

  RDdbStreamLambda
    :: Text
    -> DdbTableId
    -> (forall effs . Member DdbEff effs => a -> effs b)
    -> LambdaProfile
    -> ConfigEff LambdaId

-}
-- SQS
  RegSqsQueue
    :: Text
    -> ConfigEff SqsQueueId


-- Custom
  RegCustomResource
    :: Text
    -> (forall effs . (Member GenEff effs) => CfCustomResourceLambdaProgram effs)
    -> LambdaProfile
    -> ConfigEff CfCustomResourceId

-- CloudWatch Logs
  RegCwEventLambda
    :: Text
    -> CwEventsRuleProfile
    -> (forall effs . (Member GenEff effs) => CwLambdaProgram effs)
    -> LambdaProfile
    -> ConfigEff LambdaId


{-
-- Api
  RApi
    :: Text
    -> ConfigEff ApiId

  RApiAuthorizer
    :: Text
    -> CfCustomResourceId
    -> ApiId
    -> ConfigEff ApiAuthorizerId

  RApiResource
    :: ParentResource a
    => Text
    -> a
    -> ConfigEff ApiResourceId

  RApiMethodLambda
    :: Text
    -> ApiVerb
    -> ApiResourceId
    -> ApiMethodProfile
    -> ApiLambdaProgram
    -> LambdaProfile
    -> ConfigEff LambdaId
-}

getConfig
  :: forall effs
  .  (Member ConfigEff effs)
  => Eff effs Config
getConfig = send GetConfig

{-
getNextId
  :: forall effs a
  .  (Member ConfigEff effs, FromInt a)
  => Eff effs a
getNextId = send GetNextId
-}

genericLambda
  :: forall a b resEffs
  .  (Member ConfigEff resEffs, FromJSON a, ToJSON b)
  => Text
  -> (forall effs . (Member GenEff effs, Member S3Eff effs) => a -> Eff effs b)
  -> LambdaProfile
  -> Eff resEffs LambdaId
genericLambda name f =
  send . RegGenericLambda (Proxy :: Proxy a) (Proxy :: Proxy b) name f

s3Bucket
  :: (Member ConfigEff effs)
  => Text
  -> Eff effs S3BucketId
s3Bucket =
  send . RegS3Bucket

s3BucketLambda
  :: forall resEffs
  .  (Member ConfigEff resEffs)
  => Text
  -> S3BucketId
  -> (forall effs . (Member GenEff effs, Member S3Eff effs) => S3LambdaProgram effs)
  -> LambdaProfile
  -> Eff resEffs LambdaId
s3BucketLambda name bucketId f =
  send . RegS3BucketLambda name bucketId f

{-
ddbTable
  :: (Member ConfigEff effs)
  => Text
  -> DdbAttrDef
  -> DdbTableProfile
  -> Eff effs DdbTableId
ddbTable =
  send .:: RDdbTable

ddbStreamLambda
  :: (Member ConfigEff effs)
  => Text
  -> DdbTableId
  -> DdbStreamLambdaProgram
  -> LambdaProfile
  -> Eff effs LambdaId
ddbStreamLambda =
  send .::: RDdbStreamLambda
-}

sqsQueue
  :: (Member ConfigEff effs)
  => Text
  -> Eff effs SqsQueueId
sqsQueue =
  send . RegSqsQueue


customResource
  :: forall resEffs
  .  (Member ConfigEff resEffs)
  => Text
  -> (forall effs . (Member GenEff effs) => CfCustomResourceLambdaProgram effs)
  -> LambdaProfile
  -> Eff resEffs CfCustomResourceId
customResource name f =
  send . RegCustomResource name f

cwEventLambda
  :: forall resEffs
  .  (Member ConfigEff resEffs)
  => Text
  -> CwEventsRuleProfile
  -> (forall effs . (Member GenEff effs) => CwLambdaProgram effs)
  -> LambdaProfile
  -> Eff resEffs LambdaId
cwEventLambda name profile f =
  send . RegCwEventLambda name profile f


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

