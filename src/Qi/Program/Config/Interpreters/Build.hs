{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Config.Interpreters.Build where

import           Control.Lens                hiding (view)
import           Control.Monad.Operational
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


interpret
  :: ConfigProgram ()
  -> State Config ()
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
      let newBucket = def & s3bName .~ name
          (newBucketId, s3ConfigModifier) = insertBucket newBucket

      s3Config %= s3ConfigModifier
      return newBucketId

    rS3BucketLambda name bucketId lbdProgramFunc = do

      let newLambda = S3BucketLambda name lbdProgramFunc
          newLambdaId = LambdaId $ hash newLambda

          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll newLambdaId):)

      s3Config.s3Buckets.s3idxIdToS3Bucket %= SHM.adjust modifyBucket bucketId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId


    rDdbTable name hashAttrDef rangeAttrDef provCap = do
      let newDdbTable = DdbTable {
          _dtName         = name
        , _dtHashAttrDef  = hashAttrDef
        , _dtRangeAttrDef = rangeAttrDef
        , _dtProvCap      = provCap
        }
          (newDdbTableId, ddbConfigModifier) = insertDdbTable newDdbTable

      ddbConfig %= ddbConfigModifier
      return newDdbTableId


    rApi name = do
      let newApi = def & aName .~ name
          (newApiId, apiConfigModifier) = insertApi newApi

      apiConfig %= apiConfigModifier
      return newApiId


    rApiAuthorizer name cognitoId apiId = do
      let newApiAuthorizer = ApiAuthorizer name cognitoId apiId
          (newApiAuthorizerId, apiConfigModifier) = insertApiAuthorizer newApiAuthorizer

      apiConfig %= apiConfigModifier
      return newApiAuthorizerId


    rApiResource
      :: ParentResource a
      => Text
      -> a
      -> State Config ApiResourceId
    rApiResource name parentId = do
      let newApiResource = apiResource name $ toParentId parentId
          (newApiResourceId, apiConfigModifier) = insertApiResource newApiResource

      apiConfig %= apiConfigModifier
      return newApiResourceId


    rApiMethodLambda name verb apiResourceId auth lbdProgramFunc = do

      let newLambda = ApiLambda name lbdProgramFunc
          newLambdaId = LambdaId $ hash newLambda
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

      let newCustom = Custom newLambdaId
          (newCustomId, cfConfigModifier) = insertCustom newCustom

          newLambda = CfCustomLambda name lbdProgramFunc
          newLambdaId = LambdaId $ hash newLambda

      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      cfConfig %= cfConfigModifier

      return newCustomId

