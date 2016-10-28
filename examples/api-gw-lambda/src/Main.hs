{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

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
main = "apigwlambda" `withConfig` config
  where
    config :: ConfigProgram ()
    config = do

      api <- createApi "exampleApi"
      apiResource <- createApiResource "thing" api
      apiMethod <- createApiMethod Post apiResource

      sink <- createS3Bucket "sink"

      void $ createApiGwLambda "writeS3Object" apiMethod (writeContentsLambda sink)

    writeContentsLambda
      :: S3BucketIdentifier
      -> ApiGwEvent
      -> LambdaProgram ()
    writeContentsLambda sinkBucket event@ApiGwEvent{} = do

      content <- getRequestBody event

      -- write the content into a new file in the "output" bucket
      putS3ObjectContent outputS3Object content

      where
        outputS3Object = S3Object sinkBucket s3Key
        s3Key = S3Key "content.json"

