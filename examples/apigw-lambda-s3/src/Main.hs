{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                (def)
import           Data.Functor                (void)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           Qi                          (withConfig)
import           Qi.Config.AWS.ApiGw         (ApiMethodEvent (..),
                                              ApiVerb (Get, Post),
                                              RequestBody (JsonBody, PlainTextBody))
import           Qi.Config.AWS.S3            (S3Key (S3Key),
                                              S3Object (S3Object))
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, api,
                                              apiMethodLambda, apiResource,
                                              s3Bucket)
import           Qi.Program.Lambda.Interface (ApiLambdaProgram,
                                              getS3ObjectContent,
                                              putS3ObjectContent)
import           Qi.Util                     (success)


-- Use the curl commands below to test-drive the two endpoints (substitute your unique api stage url first):
{-
export API="https://60yaf0cfej.execute-api.us-east-1.amazonaws.com/v1"
curl -v -X POST -H "Content-Type: application/json" -d "{\"property\": 3}" "$API/things"
curl -v -X GET "$API/things"
-}



main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      bucketId  <- s3Bucket "things"

      void $ api "world" >>= \apiId ->
        apiResource "things" apiId >>= \apiResourceId -> do

          apiMethodLambda
            "createThing"
            Post apiResourceId def
            (writeContentsLambda bucketId) def

          apiMethodLambda
            "viewThing"
            Get apiResourceId def
            (readContentsLambda bucketId) def


    writeContentsLambda
      :: S3BucketId
      -> ApiLambdaProgram
    writeContentsLambda bucketId ApiMethodEvent{_aeBody} = do
      putS3ObjectContent (s3Object bucketId) content
      success "successfully added content"

      where
        content = case _aeBody of
          PlainTextBody t -> LBS.fromStrict $ encodeUtf8 t
          JsonBody v      -> encode v
          _               -> error "failed to encode request body"


    readContentsLambda
      :: S3BucketId
      -> ApiLambdaProgram
    readContentsLambda bucketId _ = do
      content <- getS3ObjectContent $ s3Object bucketId
      success . String . decodeUtf8 $ LBS.toStrict content


    s3Object = (`S3Object` s3Key)
    s3Key = S3Key "thing.json"
