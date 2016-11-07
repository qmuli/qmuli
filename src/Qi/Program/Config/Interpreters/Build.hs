{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Config.Interpreters.Build where

import           Control.Lens                hiding (view)
import           Control.Monad.Operational
import           Control.Monad.State.Strict  (State, get, put)
import           Data.Default                (def)
import           Data.Hashable               (hash)
import qualified Data.HashMap.Strict         as SHM
import           Data.Monoid                 ((<>))

import           Qi.Config.AWS
import           Qi.Config.AWS.Api
import           Qi.Config.AWS.Api.Accessors
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors
import           Qi.Config.Identifier
import           Qi.Program.Config.Interface


interpret
  :: ConfigProgram ()
  -> State Config ()
interpret program =  do
  case view program of
    (CreateS3Bucket name) :>>= is -> do
      interpret . is =<< createS3Bucket name

    (CreateS3BucketLambda name bucketId lbdProgramFunc) :>>= is -> do
      interpret . is =<< createS3BucketLambda name bucketId lbdProgramFunc

    (CreateApi name) :>>= is -> do
      interpret . is =<< createApi name

    (CreateApiRootResource name apiId) :>>= is -> do
      interpret . is =<< createApiRootResource name apiId

    (CreateApiChildResource name apiResourceId) :>>= is -> do
      interpret . is =<< createApiChildResource name apiResourceId

    (CreateApiMethodLambda name verb apiResourceId lbdProgramFunc) :>>= is -> do
      interpret . is =<< createApiMethodLambda name verb apiResourceId lbdProgramFunc


    Return _ ->
      return def

  where
    createS3Bucket name = do
      let newBucket = def & s3bName .~ name
          (newBucketId, s3ConfigModifier) = insertBucket newBucket

      s3Config %= s3ConfigModifier
      return newBucketId

    createS3BucketLambda name bucketId lbdProgramFunc = do

      let newLambda = S3BucketLambda name lbdProgramFunc
          newLambdaId = LambdaId $ hash newLambda

          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll newLambdaId):)

      s3Config.s3Buckets.s3idxIdToS3Bucket %= SHM.adjust modifyBucket bucketId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId



    createApi name = do
      let newApi = def & aName .~ name
          (newApiId, apiConfigModifier) = insertApi newApi

      apiConfig %= apiConfigModifier
      return newApiId

    createApiRootResource name apiId =
      createApiResource name $ Left apiId

    createApiChildResource name apiResourceId =
      createApiResource name $ Right apiResourceId

    createApiResource name parentId = do
      let newApiResource = apiResource name parentId
          (newApiResourceId, apiConfigModifier) = insertApiResource newApiResource

      apiConfig %= apiConfigModifier
      return newApiResourceId


    createApiMethodLambda name verb apiResourceId lbdProgramFunc = do

      let newLambda = ApiLambda name lbdProgramFunc
          newLambdaId = LambdaId $ hash newLambda
          modifyApiResource = arMethodConfigs %~ ((ApiMethodConfig verb newLambdaId):)

      apiConfig.acApiResources %= SHM.adjust modifyApiResource apiResourceId
      lbdConfig.lcLambdas %= SHM.insert newLambdaId newLambda
      return newLambdaId

