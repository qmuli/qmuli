{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}


module Qi.Program.Config.Interpreters.Build where

import           Control.Lens                hiding (view)
import           Control.Monad.Operational   (ProgramViewT ((:>>=), Return),
                                              view)
import           Control.Monad.State.Class   (MonadState)
import           Control.Monad.State.Strict  (State)
import           Data.Default                (def)
import qualified Data.HashMap.Strict         as SHM
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)

import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.CF
import qualified Qi.Config.AWS.CF.Accessors  as CF
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Config.Interface hiding (apiResource)

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
    (RS3Bucket name) :>>= is ->
      interpret . is =<< rS3Bucket name

    (RS3BucketLambda name bucketId lbdProgramFunc profile) :>>= is ->
      interpret . is =<< rS3BucketLambda name bucketId lbdProgramFunc profile

    (RDdbTable name hashAttrDef profile) :>>= is ->
      interpret . is =<< rDdbTable name hashAttrDef profile

    (RApi name) :>>= is ->
      interpret . is =<< rApi name

    (RApiAuthorizer name cognitoId apiId) :>>= is ->
      interpret . is =<< rApiAuthorizer name cognitoId apiId

    (RApiResource name parentId) :>>= is ->
      interpret . is =<< rApiResource name parentId

    (RApiMethodLambda name verb apiResourceId methodProfile lbdProgramFunc lbdProfile) :>>= is ->
      interpret . is =<< rApiMethodLambda name verb apiResourceId methodProfile lbdProgramFunc lbdProfile

    (RCustomResource name lbdProgramFunc profile) :>>= is ->
      interpret . is =<< rCustomResource name lbdProgramFunc profile

    (RCwEventLambda name ruleProfile lbdProgramFunc lbdProfile) :>>= is ->
      interpret . is =<< rCwEventLambda name ruleProfile lbdProgramFunc lbdProfile

    Return _ ->
      return def

  where
    rS3Bucket name = do
      newS3BucketId <- getNextId
      let newBucket = def & s3bName .~ name
          insertIdToS3Bucket = s3idxIdToS3Bucket %~ SHM.insert newS3BucketId newBucket
          insertNameToId = s3idxNameToId %~ SHM.insert name newS3BucketId

      s3Config . s3Buckets %= insertNameToId . insertIdToS3Bucket
      return newS3BucketId


    rS3BucketLambda name bucketId lbdProgramFunc profile = do

      newLambdaId <- getNextId
      let newLambda = S3BucketLambda name profile lbdProgramFunc
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll newLambdaId):)

      s3Config.s3Buckets.s3idxIdToS3Bucket %= SHM.adjust modifyBucket bucketId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId


    rDdbTable name hashAttrDef profile = do
      newDdbTableId <- getNextId
      let newDdbTable = DdbTable {
          _dtName         = name
        , _dtHashAttrDef  = hashAttrDef
        , _dtProfile      = profile
        }

      ddbConfig . dcTables %= SHM.insert newDdbTableId newDdbTable
      return newDdbTableId


    rApi name = do
      newApiId <- getNextId
      let newApi = def & aName .~ name
          insertIdToApi = acApis %~ SHM.insert newApiId newApi
          insertIdToApiResourceDeps = acApiResourceDeps %~ SHM.insert (Left newApiId) []
          insertIdToApiAuthorizerDeps = acApiAuthorizerDeps %~ SHM.insert newApiId []

      apiGwConfig %= insertIdToApi . insertIdToApiResourceDeps . insertIdToApiAuthorizerDeps
      return newApiId


    rApiAuthorizer name cognitoId apiId = do
      newApiAuthorizerId <- getNextId
      let newApiAuthorizer = ApiAuthorizer name cognitoId apiId
          insertIdToApiAuthorizer = acApiAuthorizers %~ SHM.insert newApiAuthorizerId newApiAuthorizer
          insertIdToApiAuthorizerDeps = acApiAuthorizerDeps %~ SHM.unionWith (++) (SHM.singleton apiId [newApiAuthorizerId])

      apiGwConfig %= insertIdToApiAuthorizer . insertIdToApiAuthorizerDeps
      return newApiAuthorizerId


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
      return newApiResourceId


    rApiMethodLambda name verb apiResourceId methodProfile lbdProgramFunc lbdProfile = do
      newLambdaId <- getNextId
      let newLambda = ApiLambda name lbdProfile lbdProgramFunc
          modifyApiResource = arMethodConfigs %~ (apiMethodConfig:)
          apiMethodConfig = ApiMethodConfig {
              amcVerb     = verb
            , amcProfile  = methodProfile
            , amcLbdId    = newLambdaId
            }

      apiGwConfig.acApiResources %= SHM.adjust modifyApiResource apiResourceId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId


    rCustomResource name lbdProgramFunc profile = do
      newLambdaId <- getNextId
      let newCustom = Custom newLambdaId
          (newCustomId, cfConfigModifier) = CF.insert newCustom

          newLambda = CfCustomLambda name profile lbdProgramFunc

      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      cfConfig %= cfConfigModifier

      return newCustomId


    rCwEventLambda name ruleProfile lbdProgramFunc lbdProfile = do
      newEventsRuleId <- getNextId
      newLambdaId     <- getNextId

      let newLambda = CwEventLambda name lbdProfile lbdProgramFunc
          newEventsRule = CwEventsRule {
            _cerName    = name
          , _cerProfile = ruleProfile
          , _cerLbdId   = newLambdaId
          }

      cwConfig.ccRules %= SHM.insert newEventsRuleId newEventsRule
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId


