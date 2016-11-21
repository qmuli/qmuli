{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad               (void)
import           Data.Aeson
import qualified Data.ByteString.Lazy        as LBS
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           Qi                          (withConfig)
import           Qi.Config.AWS.Api           (ApiEvent (..),
                                              ApiVerb (Get, Post),
                                              RequestBody (JsonBody, PlainTextBody))
import           Qi.Config.AWS.S3            (S3Key (S3Key),
                                              S3Object (S3Object))
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, api,
                                              apiMethodLambda, apiRootResource,
                                              s3Bucket)
import           Qi.Program.Lambda.Interface (LambdaProgram, getS3ObjectContent,
                                              putS3ObjectContent, respond)
import           Qi.Util.Api


-- Used the two curl commands below to test-drive the two endpoints (substitute your unique api stage url first):
--
-- curl -v -X POST -H "Content-Type: application/json" -d "{\"property\": 3}" "https://50nk1jhzda.execute-api.us-east-1.amazonaws.com/v1/things"
-- curl -v -X GET "https://50nk1jhzda.execute-api.us-east-1.amazonaws.com/v1/things"
--

main :: IO ()
main =
  "apigwlambdas3" `withConfig` config

    where
      config :: ConfigProgram ()
      config = do
        bucketId  <- s3Bucket "things"

        api "world" >>= \apiId ->
          apiRootResource "things" apiId >>= \apiResourceId -> do

            apiMethodLambda
              "createThing"
              Post
              apiResourceId
              $ writeContentsLambda bucketId

            apiMethodLambda
              "viewThing"
              Get
              apiResourceId
              $ readContentsLambda bucketId

        return ()


      writeContentsLambda
        :: S3BucketId
        -> ApiEvent
        -> LambdaProgram ()
      writeContentsLambda bucketId event@ApiEvent{_aeBody} = do
        putS3ObjectContent (s3Object bucketId) content
        successString "successfully added content"

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
        success . String . decodeUtf8 $ LBS.toStrict content


      s3Object = (`S3Object` s3Key)
      s3Key = S3Key "thing.json"
