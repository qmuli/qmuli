{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Program.Config.Ipret.Build where

import           Control.Lens                             hiding (view)
import           Control.Monad.Freer
import           Control.Monad.State.Class                (MonadState)
import           Control.Monad.State.Strict               (State)
import           Data.Default                             (def)
import qualified Data.HashMap.Strict                      as SHM
import           Data.Proxy                               (Proxy (Proxy))
import           Protolude                                hiding (State)
import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.CF
import qualified Qi.Config.AWS.CfCustomResource.Accessors as CustomResource
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.SQS
import           Qi.Config.Identifier
import           Qi.Program.Config.Lang


newtype QiConfig a = QiConfig {unQiConfig :: State Config a}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadState Config
    )

run
  :: Eff '[ResEff, QiConfig] a -> QiConfig a
run = runM . interpret (\case

  RGenericLambda name programFunc profile -> send . QiConfig $
    withNextId $ \id -> do
      let newLambda = GenericLambda name profile programFunc Proxy Proxy
      lbdConfig.lcLambdas %= SHM.insert id newLambda

-- S3
  RS3Bucket name -> send . QiConfig $
    withNextId $ \id -> do
      let newBucket = def & s3bName .~ name
          insertIdToS3Bucket = s3idxIdToS3Bucket %~ SHM.insert id newBucket
          insertNameToId = s3idxNameToId %~ SHM.insert name id
      s3Config . s3Buckets %= insertNameToId . insertIdToS3Bucket

  RS3BucketLambda name bucketId programFunc profile -> send . QiConfig $
    withNextId $ \id -> do
      let newLambda = S3BucketLambda name profile programFunc
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll id):)
      s3Config.s3Buckets.s3idxIdToS3Bucket %= SHM.adjust modifyBucket bucketId
      lbdConfig.lcLambdas %= SHM.insert id newLambda


-- DDB
  RDdbTable name hashAttrDef profile -> send . QiConfig $
    withNextId $ \id -> do
      let newDdbTable = DdbTable {
          _dtName           = name
        , _dtHashAttrDef    = hashAttrDef
        , _dtProfile        = profile
        , _dtStreamHandler  = Nothing
        }
      ddbConfig . dcTables %= SHM.insert id newDdbTable


  RDdbStreamLambda name tableId programFunc profile -> send . QiConfig $
    withNextId $ \id -> do
      let newLambda = DdbStreamLambda name profile programFunc

      ddbConfig.dcTables %= SHM.adjust (dtStreamHandler .~ Just id) tableId
      lbdConfig.lcLambdas %= SHM.insert id newLambda


-- Sqs
  RSqsQueue name -> send . QiConfig $
    withNextId $ \id -> do
      sqsConfig . sqsQueues %= SHM.insert id (SqsQueue{ _sqsQueueName = name })


-- Custom
  RCustomResource name programFunc profile -> send . QiConfig $ do
    id <- getNextId
    let newCustomResource = CfCustomResource id
        (newCustomId, cfConfigModifier) = CustomResource.insert newCustomResource

        newLambda = CfCustomLambda name profile programFunc

    lbdConfig . lcLambdas %= SHM.insert id newLambda
    cfConfig %= cfConfigModifier
    pure newCustomId


-- CloudWatch
  RCwEventLambda name ruleProfile programFunc profile -> send . QiConfig $ do
    newEventsRuleId <- getNextId
    withNextId $ \newLambdaId -> do
      let newLambda = CwEventLambda name profile programFunc
          newEventsRule = CwEventsRule {
            _cerName    = name
          , _cerProfile = ruleProfile
          , _cerLbdId   = newLambdaId
          }

      cwConfig . ccRules %= SHM.insert newEventsRuleId newEventsRule
      lbdConfig . lcLambdas %= SHM.insert newLambdaId newLambda

  )


  where
    withNextId f = do
      id <- getNextId
      f id
      pure id


{-

-- Api
    rApi name = do
      newApiId <- getNextId
      let newApi = def & aName .~ name
          insertIdToApi = acApis %~ SHM.insert newApiId newApi
          insertIdToApiResourceDeps = acApiResourceDeps %~ SHM.insert (Left newApiId) []
          insertIdToApiAuthorizerDeps = acApiAuthorizerDeps %~ SHM.insert newApiId []

      apiGwConfig %= insertIdToApi . insertIdToApiResourceDeps . insertIdToApiAuthorizerDeps
      pure newApiId


    rApiAuthorizer name cognitoId apiId = do
      newApiAuthorizerId <- getNextId
      let newApiAuthorizer = ApiAuthorizer name cognitoId apiId
          insertIdToApiAuthorizer = acApiAuthorizers %~ SHM.insert newApiAuthorizerId newApiAuthorizer
          insertIdToApiAuthorizerDeps = acApiAuthorizerDeps %~ SHM.unionWith (++) (SHM.singleton apiId [newApiAuthorizerId])

      apiGwConfig %= insertIdToApiAuthorizer . insertIdToApiAuthorizerDeps
      pure newApiAuthorizerId


    rApiResource
      :: ParentResource a
      => Text
      -> a
      -> QiConfig ApiResourceId
    rApiResource name pid = do
      newApiResourceId <- getNextId
      let parentId = toParentId pid
          newApiResource = apiResource name parentId
          insertIdToApiResource = acApiResources %~ SHM.insert newApiResourceId newApiResource
          insertIdToApiResourceDeps = acApiResourceDeps %~ SHM.unionWith (++) (SHM.singleton parentId [newApiResourceId])

      apiGwConfig %= insertIdToApiResource . insertIdToApiResourceDeps
      pure newApiResourceId


    rApiMethodLambda name verb apiResourceId methodProfile programFunc profile = do
      newLambdaId <- getNextId
      let newLambda = ApiLambda name profile programFunc
          apiMethodConfig = ApiMethodConfig {
              amcVerb     = verb
            , amcProfile  = methodProfile
            , amcLbdId    = newLambdaId
            }

      apiGwConfig . acApiResources %= SHM.adjust (arMethodConfigs %~ (apiMethodConfig:)) apiResourceId
      lbdConfig . lcLambdas %= SHM.insert newLambdaId newLambda
      pure newLambdaId

-}

