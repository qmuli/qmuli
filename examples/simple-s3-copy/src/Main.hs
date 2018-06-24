{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Data.Default                (def)
import           Protolude
import           Qi                          (withConfig)
import           Qi.Config.AWS.Lambda        (LambdaMemorySize (..),
                                              lpMemorySize)
import           Qi.Config.AWS.S3            (S3Event, s3Object, s3eObject,
                                              s3oBucketId, s3oKey)
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, s3Bucket,
                                              s3BucketLambda)
import           Qi.Program.Lambda.Interface (S3LambdaProgram,
                                              getS3ObjectContent,
                                              putS3ObjectContent, say)


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do

      -- create an "input" s3 bucket
      incoming <- s3Bucket "incoming"

      -- create an "output" s3 bucket
      outgoing <- s3Bucket "outgoing"

      -- create a lambda, which will copy an s3 object from "incoming" to "outgoing" buckets
      -- upon an S3 "Put" event.
      -- Attach the lambda to the "incoming" bucket such way so each time a file is uploaded to
      -- the bucket, the lambda is called with the information about the newly uploaded file.
      -- The lambda creation function takes the Lambda name, s3BucketId to attach to, lambda
      -- function itself and a lambda profile, that specifies attributes like memory size and
      -- timeout, and has meaningful defaults for those.
      void $ s3BucketLambda "copyS3Object" incoming (copyContentsLambda outgoing) $
        def & lpMemorySize .~ M1536

    copyContentsLambda
      :: S3BucketId
      -> S3LambdaProgram
    copyContentsLambda sinkBucketId = lbd
      where
        lbd event = do
          let incomingS3Obj = event ^. s3eObject
              outgoingS3Obj = s3oBucketId .~ sinkBucketId $ incomingS3Obj

          -- get the content of the newly uploaded file
          eitherContent <- getS3ObjectContent incomingS3Obj

          case eitherContent of
            Right content -> do
              -- write the content into a new file in the "output" bucket
              putS3ObjectContent outgoingS3Obj content

              pure "lambda had executed successfully"

            Left err ->
              pure . toS $ "error: '" <> err <> "'"

