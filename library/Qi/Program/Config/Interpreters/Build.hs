{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Qi.Program.Config.Interpreters.Build where

import           Control.Lens                             hiding (view)
import           Control.Monad.Operational                (ProgramViewT ((:>>=), Return),
                                                           view)
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
import           Qi.Program.Config.Interface              hiding (apiResource)


newtype QiConfig a = QiConfig {unQiConfig :: State Config a}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadState Config
    )

interpret
  :: ConfigProgram ()
  -> QiConfig ()
interpret program =
  case view program of

    RGenericLambda name programFunc profile :>>= is ->
      interpret . is =<< rGenericLambda name programFunc profile

    RS3Bucket name :>>= is ->
      interpret . is =<< rS3Bucket name

    RS3BucketLambda name bucketId programFunc profile :>>= is ->
      interpret . is =<< rS3BucketLambda name bucketId programFunc profile

    RDdbTable name hashAttrDef profile :>>= is ->
      interpret . is =<< rDdbTable name hashAttrDef profile

    RDdbStreamLambda name tableId lbd profile :>>= is ->
      interpret . is =<< rDdbStreamLambda name tableId lbd profile

    RApi name :>>= is ->
      interpret . is =<< rApi name

    RApiAuthorizer name cognitoId apiId :>>= is ->
      interpret . is =<< rApiAuthorizer name cognitoId apiId

    RApiResource name parentId :>>= is ->
      interpret . is =<< rApiResource name parentId

    RApiMethodLambda name verb apiResourceId methodProfile programFunc profile :>>= is ->
      interpret . is =<< rApiMethodLambda name verb apiResourceId methodProfile programFunc profile

    RCustomResource name programFunc profile :>>= is ->
      interpret . is =<< rCustomResource name programFunc profile

    RCwEventLambda name ruleProfile programFunc profile :>>= is ->
      interpret . is =<< rCwEventLambda name ruleProfile programFunc profile

    RSqsQueue name :>>= is ->
      interpret . is =<< rSqsQueue name

    Return _ ->
      pure def

  where

-- Lambda
    rGenericLambda name programFunc profile = do
      newLambdaId <- getNextId
      let newLambda = GenericLambda name profile programFunc Proxy Proxy
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      pure newLambdaId

-- S3
    rS3Bucket name = do
      newS3BucketId <- getNextId
      let newBucket = def & s3bName .~ name
          insertIdToS3Bucket = s3idxIdToS3Bucket %~ SHM.insert newS3BucketId newBucket
          insertNameToId = s3idxNameToId %~ SHM.insert name newS3BucketId

      s3Config . s3Buckets %= insertNameToId . insertIdToS3Bucket
      pure newS3BucketId


    rS3BucketLambda name bucketId programFunc profile = do
      newLambdaId <- getNextId
      let newLambda = S3BucketLambda name profile programFunc
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll newLambdaId):)

      s3Config.s3Buckets.s3idxIdToS3Bucket %= SHM.adjust modifyBucket bucketId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      pure newLambdaId

-- DDB
    rDdbTable name hashAttrDef profile = do
      newDdbTableId <- getNextId
      let newDdbTable = DdbTable {
          _dtName           = name
        , _dtHashAttrDef    = hashAttrDef
        , _dtProfile        = profile
        , _dtStreamHandler  = Nothing
        }

      ddbConfig . dcTables %= SHM.insert newDdbTableId newDdbTable
      pure newDdbTableId


    rDdbStreamLambda name tableId programFunc profile = do
      newLambdaId <- getNextId
      let newLambda = DdbStreamLambda name profile programFunc

      ddbConfig.dcTables %= SHM.adjust (dtStreamHandler .~ Just newLambdaId) tableId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      pure newLambdaId

-- Sqs
    rSqsQueue name = do
      id <- getNextId
      sqsConfig . sqsQueues %= SHM.insert id (SqsQueue{ _sqsQueueName = name })
      pure id


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


    rCustomResource name programFunc profile = do
      newLambdaId <- getNextId
      let newCustomResource = CfCustomResource newLambdaId
          (newCustomId, cfConfigModifier) = CustomResource.insert newCustomResource

          newLambda = CfCustomLambda name profile programFunc

      lbdConfig . lcLambdas %= SHM.insert newLambdaId newLambda
      cfConfig %= cfConfigModifier

      pure newCustomId


    rCwEventLambda name ruleProfile programFunc profile = do
      newEventsRuleId <- getNextId
      newLambdaId     <- getNextId

      let newLambda = CwEventLambda name profile programFunc
          newEventsRule = CwEventsRule {
            _cerName    = name
          , _cerProfile = ruleProfile
          , _cerLbdId   = newLambdaId
          }

      cwConfig . ccRules %= SHM.insert newEventsRuleId newEventsRule
      lbdConfig . lcLambdas %= SHM.insert newLambdaId newLambda
      pure newLambdaId


