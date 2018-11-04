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

module Qi.Program.Config.Ipret.State where

import           Control.Lens                             hiding (view)
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Default                             (def)
import qualified Data.HashMap.Strict                      as SHM
import           Data.Proxy                               (Proxy (Proxy))
import           Protolude                                hiding (State, get,
                                                           gets, modify,
                                                           runState)
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
import           Qi.Program.Config.Lang                   (ConfigEff (..))


run
  :: forall effs a
  .  (Member (State Config) effs)
  => Eff (ConfigEff ': effs) a -> Eff effs a
run = interpret (\case

  GetConfig -> get

-- get a resource-specific identifier based on the next autoincremented numeric id
-- while keeping the autoincrement state in the global Config
  {- GetNextId -> getNextId -}


  RegGenericLambda inProxy outProxy name f profile ->
    withNextId (Proxy :: Proxy LambdaId) $ \id -> do
      let lbd = GenericLambda name profile inProxy outProxy f
      insertLambda id name lbd

-- S3
  RegS3Bucket name profile -> do
    withNextId (Proxy :: Proxy S3BucketId) $ \id -> do
      let newBucket = def & s3bName .~ name
                          & s3bProfile .~ profile
          insertIdToS3Bucket = s3IdToBucket %~ SHM.insert id newBucket
          insertNameToId = s3NameToBucketId %~ SHM.insert name id

      modify (s3Config . s3Buckets %~ insertNameToId . insertIdToS3Bucket)


  RegS3BucketLambda name bucketId f profile ->
    withNextId (Proxy :: Proxy LambdaId) $ \id -> do
      let lbd = S3BucketLambda name profile f
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll id):)
      modify (s3Config . s3Buckets . s3IdToBucket %~ SHM.adjust modifyBucket bucketId)

      insertLambda id name lbd

{-
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
      let lbd = DdbStreamLambda name profile programFunc

      ddbConfig.dcTables %= SHM.adjust (dtStreamHandler .~ Just id) tableId

      lbdConfig.lcLambdas %= SHM.insert id lbd

      insertLambda id name lbd

-}
-- Sqs
  RegSqsQueue name ->
    withNextId (Proxy :: Proxy SqsQueueId) $ \id -> do
      modify (sqsConfig . sqsQueues %~ SHM.insert id SqsQueue{ _sqsQueueName = name })


-- Custom
  RegCustomResource name programFunc profile -> do
    id <- getNextId
    let customResource = CfCustomResource id
        (newCustomId, cfConfigModifier) = CustomResource.insert customResource
        lbd = CfCustomLambda name profile programFunc

    insertLambda id name lbd

    modify $ cfConfig %~ cfConfigModifier
    pure newCustomId


-- CloudWatch
  RegCwEventLambda name ruleProfile programFunc profile -> do
    eventsRuleId <- getNextId
    withNextId (Proxy :: Proxy LambdaId) $ \id -> do
      let lbd = CwEventLambda name profile programFunc
          eventsRule = CwEventsRule {
            _cerName    = name
          , _cerProfile = ruleProfile
          , _cerLbdId   = id
          }

      modify $ cwConfig . ccRules %~ SHM.insert eventsRuleId eventsRule
      insertLambda id name lbd
  )


  where

    getNextId
      :: FromInt id
      => Eff effs id
    getNextId = do
      id <- gets (fromInt . (^. nextId))
      modify (nextId %~ (+1))
      pure id

    withNextId
      :: FromInt id
      => Proxy id
      -> (id -> Eff effs c)
      -> Eff effs id
    withNextId _ f = do
      id <- gets (fromInt . (^. nextId))
      modify (nextId %~ (+1))
      f id
      pure id

    insertLambda
      :: LambdaId
      -> Text
      -> Lambda
      -> Eff effs ()
    insertLambda id name lbd = do

      let insertIdToLambda  = lbdIdToLambda %~ SHM.insert id lbd
          insertNameToId    = lbdNameToId   %~ SHM.insert name id

      void $ modify (lbdConfig %~ insertNameToId . insertIdToLambda)

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

