{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad               (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy        as LBS
import           Data.Text.Encoding          (encodeUtf8)
import           Qi                          (withConfig)
import           Qi.Config.AWS.Api           (ApiEvent (..),
                                              ApiVerb (Get, Post),
                                              RequestBody (JsonBody, PlainTextBody))
import           Qi.Config.AWS.S3            (S3Key (S3Key),
                                              S3Object (S3Object))
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, createApi,
                                              createApiMethodLambda,
                                              createApiRootResource,
                                              createS3Bucket)
import           Qi.Program.Lambda.Interface (LambdaProgram, getS3ObjectContent,
                                              output, putS3ObjectContent)

-- Used the two curl commands below to test-drive the two endpoints (substitute your unique api stage url first):
--
-- curl -v -X POST -H "Content-Type: application/json" -d "{\"testvalue\": 3}" "https://ns45qabsx8.execute-api.us-east-1.amazonaws.com/v1/things"
-- curl -v -X GET "https://ns45qabsx8.execute-api.us-east-1.amazonaws.com/v1/things"
--

main :: IO ()
main =
  "apigwlambda" `withConfig` config

    where
      config :: ConfigProgram ()
      config = do
        bucketId  <- createS3Bucket "things"

        apiId         <- createApi "world"
        apiResourceId <- createApiRootResource "things" apiId

        void $ createApiMethodLambda
          "createThing"
          Post
          apiResourceId
          $ writeContentsLambda bucketId

        void $ createApiMethodLambda
          "viewThing"
          Get
          apiResourceId
          $ readContentsLambda bucketId


      writeContentsLambda
        :: S3BucketId
        -> ApiEvent
        -> LambdaProgram ()
      writeContentsLambda bucketId event@ApiEvent{_aeBody} = do
        putS3ObjectContent (s3Object bucketId) content
        output "successfully added content"

        where
          content = case _aeBody of
            PlainTextBody t -> LBS.fromStrict $ encodeUtf8 t
            JsonBody v      -> encode v


      readContentsLambda
        :: S3BucketId
        -> ApiEvent
        -> LambdaProgram ()
      readContentsLambda bucketId _ = do
        content <- getS3ObjectContent $ s3Object bucketId
        output $ LBS.toStrict content


      s3Object = (`S3Object` s3Key)
      s3Key = S3Key "thing.json"
