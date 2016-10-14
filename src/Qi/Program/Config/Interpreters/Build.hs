{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Config.Interpreters.Build where

import           Control.Lens
import           Control.Monad.Operational
import           Control.Monad.State.Strict  (State, get, put)
import           Data.Default                (def)
import           Data.Hashable               (hash)
import qualified Data.HashMap.Strict         as SHM
import           Data.Monoid                 ((<>))

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Config.Interface (ConfigInstruction (CreateS3Bucket, CreateS3BucketLambda),
                                              ConfigProgram)


interpret
  :: ConfigProgram ()
  -> State Config ()
interpret config =  do
  i <- viewT config
  case i of
    (CreateS3Bucket name) :>>= is -> do
      interpret . is =<< createS3Bucket name

    (CreateS3BucketLambda name bucketId lbdProgramFunc) :>>= is -> do
      interpret . is =<< createS3BucketLambda name bucketId lbdProgramFunc

    Return _ ->
      return def


  where

    createS3Bucket name = do
      conf <- get

      let newBucket = S3Bucket {
            _bucketName = name
          , _lbdEventConfigs = []
          }
          newBucketIdentifier = S3BucketIdentifier $ hash newBucket

      put $ conf <> def{_s3Config = def{
            _s3Buckets = SHM.singleton newBucketIdentifier newBucket
          }
        }

      return newBucketIdentifier


    createS3BucketLambda name bucketId lbdProgramFunc = do
      conf <- get

      let newLambda = S3BucketLambda name bucketId lbdProgramFunc
          newLambdaIdentifier = LambdaIdentifier $ hash newLambda

          modifyBucket = over lbdEventConfigs ((LambdaEventConfig S3ObjectCreatedAll newLambdaIdentifier):)
          modifiedConf = over (s3Config . s3Buckets) (SHM.adjust modifyBucket bucketId) conf

      put $ modifiedConf <> def{_lbdConfig = def{_lcLambdas = SHM.singleton newLambdaIdentifier newLambda}}

      return newLambdaIdentifier
