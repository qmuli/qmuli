{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad               (void)

import           Qi                          (withConfig)
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier        (S3BucketIdentifier)
import           Qi.Program.Config.Interface (ConfigProgram, createS3Bucket,
                                              createS3BucketLambda)
import           Qi.Program.Lambda.Interface (LambdaProgram, getS3ObjectContent,
                                              putS3ObjectContent)


main :: IO ()
main = do
  -- Qmulus name must be globaly unique as the main deployment S3 bucket uses this name and
  -- all underlying resource names use it as a prefix. To avoid collisions, rename this demo:
  --
  --   "myfancyuniquelynamedproject" `withConfig` config
  --
  "simples3copy" `withConfig` config

  where
    config :: ConfigProgram ()
    config = do

      -- create an "input" s3 bucket
      incoming <- createS3Bucket "incoming"

      -- create an "output" s3 bucket
      outgoing <- createS3Bucket "outgoing"

      -- create a lambda, which will copy an s3 object from "input" to "output" buckets
      -- upon an S3 "Put" event.
      -- Attach the lambda to the "input" bucket such way so each time a file is uploaded to
      -- the bucket, the lambda is called with the information about the newly uploaded file.
      void $ createS3BucketLambda "copyS3Object" incoming (copyContentsLambda outgoing)

    copyContentsLambda
      :: S3BucketIdentifier
      -> S3Event
      -> LambdaProgram ()
    copyContentsLambda sinkBucket S3Event{s3Object = s3Obj@S3Object{s3oKey = s3Key}} = do

      -- get the content of the newly uploaded file
      content <- getS3ObjectContent s3Obj

      -- write the content into a new file in the "output" bucket
      putS3ObjectContent outputS3Object content

      where
        outputS3Object = S3Object sinkBucket s3Key
