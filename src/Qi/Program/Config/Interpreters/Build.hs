{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}


module Qi.Program.Config.Interpreters.Build where

import           Control.Lens                hiding (view)
import           Control.Monad.Operational
import           Control.Monad.Random
import           Control.Monad.Random.Class  (MonadRandom)
import           Control.Monad.State.Class   (MonadState)
import           Control.Monad.State.Strict  (State)
import           Data.Default                (def)
import           Data.Hashable               (hash)
import qualified Data.HashMap.Strict         as SHM
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)

import           Qi.Config.AWS
import           Qi.Config.AWS.Api
import           Qi.Config.AWS.Api.Accessors
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CF.Accessors
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.DDB.Accessors
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors
import           Qi.Config.Identifier
import           Qi.Program.Config.Interface hiding (apiResource)


newtype QiConfig a = QiConfig {unQiConfig :: RandT StdGen (State Config) a}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadState Config
    , MonadRandom
    )

interpret
  :: ConfigProgram ()
  -> QiConfig ()
interpret program =  do
  case view program of
    (RS3Bucket name) :>>= is -> do
      interpret . is =<< rS3Bucket name

    (RS3BucketLambda name bucketId lbdProgramFunc) :>>= is -> do
      interpret . is =<< rS3BucketLambda name bucketId lbdProgramFunc

    (RDdbTable name hashAttrDef rangeAttrDef provCap) :>>= is -> do
      interpret . is =<< rDdbTable name hashAttrDef rangeAttrDef provCap

    (RApi name) :>>= is -> do
      interpret . is =<< rApi name

    (RApiAuthorizer name cognitoId apiId) :>>= is -> do
      interpret . is =<< rApiAuthorizer name cognitoId apiId

    (RApiResource name parentId) :>>= is -> do
      interpret . is =<< rApiResource name parentId

    (RApiMethodLambda name verb apiResourceId auth lbdProgramFunc) :>>= is -> do
      interpret . is =<< rApiMethodLambda name verb apiResourceId auth lbdProgramFunc

    (RCustomResource name lbdProgramFunc) :>>= is -> do
      interpret . is =<< rCustomResource name lbdProgramFunc

    Return _ ->
      return def

  where
    rS3Bucket name = do
      newS3BucketId <- getRandom
      let newBucket = def & s3bName .~ name
          insertIdToS3Bucket = s3idxIdToS3Bucket %~ SHM.insert newS3BucketId newBucket
          insertNameToId = s3idxNameToId %~ SHM.insert name newS3BucketId

      s3Config . s3Buckets %= insertNameToId . insertIdToS3Bucket
      return newS3BucketId


    rS3BucketLambda name bucketId lbdProgramFunc = do

      newLambdaId <- getRandom
      let newLambda = S3BucketLambda name lbdProgramFunc
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll newLambdaId):)

      s3Config.s3Buckets.s3idxIdToS3Bucket %= SHM.adjust modifyBucket bucketId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId


    rDdbTable name hashAttrDef rangeAttrDef provCap = do
      newDdbTableId <- DdbTableId <$> getRandom
      let newDdbTable = DdbTable {
          _dtName         = name
        , _dtHashAttrDef  = hashAttrDef
        , _dtRangeAttrDef = rangeAttrDef
        , _dtProvCap      = provCap
        }

      ddbConfig . dcTables %= SHM.insert newDdbTableId newDdbTable
      return newDdbTableId


    rApi name = do
      newApiId <- getRandom
      let newApi = def & aName .~ name
          insertIdToApi = acApis %~ SHM.insert newApiId newApi
          insertIdToApiResourceDeps = acApiResourceDeps %~ SHM.insert (Left newApiId) []
          insertIdToApiAuthorizerDeps = acApiAuthorizerDeps %~ SHM.insert newApiId []

      apiConfig %= insertIdToApi . insertIdToApiResourceDeps . insertIdToApiAuthorizerDeps
      return newApiId


    rApiAuthorizer name cognitoId apiId = do
      newApiAuthorizerId <- getRandom
      let newApiAuthorizer = ApiAuthorizer name cognitoId apiId
          insertIdToApiAuthorizer = acApiAuthorizers %~ SHM.insert newApiAuthorizerId newApiAuthorizer
          insertIdToApiAuthorizerDeps = acApiAuthorizerDeps %~ SHM.unionWith (++) (SHM.singleton apiId [newApiAuthorizerId])

      apiConfig %= insertIdToApiAuthorizer . insertIdToApiAuthorizerDeps
      return newApiAuthorizerId


    rApiResource
      :: ParentResource a
      => Text
      -> a
      -> QiConfig ApiResourceId
    rApiResource name pid = do
      newApiResourceId <- getRandom
      let parentId = toParentId pid
          newApiResource = apiResource name parentId
          insertIdToApiResource = acApiResources %~ SHM.insert newApiResourceId newApiResource
          insertIdToApiResourceDeps = acApiResourceDeps %~ SHM.unionWith (++) (SHM.singleton parentId [newApiResourceId])

      apiConfig %= insertIdToApiResource . insertIdToApiResourceDeps
      return newApiResourceId


    rApiMethodLambda name verb apiResourceId auth lbdProgramFunc = do
      newLambdaId <- getRandom
      let newLambda = ApiLambda name lbdProgramFunc
          modifyApiResource = arMethodConfigs %~ (apiMethodConfig:)
          apiMethodConfig = ApiMethodConfig {
              amcVerb = verb
            , amcAuthId = auth
            , amcLbdId = newLambdaId
            }

      apiConfig.acApiResources %= SHM.adjust modifyApiResource apiResourceId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId


    rCustomResource name lbdProgramFunc = do
      newLambdaId <- getRandom
      let newCustom = Custom newLambdaId
          (newCustomId, cfConfigModifier) = insertCustom newCustom

          newLambda = CfCustomLambda name lbdProgramFunc

      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      cfConfig %= cfConfigModifier

      return newCustomId

