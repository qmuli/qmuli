{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Assets where

import           Control.Monad               (void)

import           Qi.Config.AWS.S3
import           Qi.Config.Identifier        (S3BucketIdentifier)
import           Qi.Program.Config.Interface (ConfigProgram, createS3Bucket,
                                              createS3BucketLambda)
import           Qi.Program.Lambda.Interface


config :: ConfigProgram ()
config = do
  incoming <- createS3Bucket "incoming"
  outgoing <- createS3Bucket "outgoing"
  void $ createS3BucketLambda "copyS3Object" incoming (copyContentsLambda outgoing)

copyContentsLambda
  :: S3BucketIdentifier
  -> S3Event
  -> LambdaProgram ()
copyContentsLambda sinkBucket S3Event{s3Object} = do
  content <- getS3ObjectContent s3Object
  putS3ObjectContent outputS3Object content

  where
    outputS3Object = S3Object sinkBucket (S3Key "out")
