{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)

import           Data.Text                   (pack)
import           Qi                          (withConfig)
import           Qi.Config.AWS.S3            (S3Event, s3Object, s3eObject,
                                              s3oBucketId, s3oKey)
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, s3Bucket,
                                              s3BucketLambda)
import           Qi.Program.Lambda.Interface (LambdaProgram, getS3ObjectContent,
                                              putS3ObjectContent)
import           System.Environment          (getArgs, withArgs)

main :: IO ()
main =  do
  args <- getArgs
  case args of
    (appName:rest) -> withArgs rest $ (pack appName) `withConfig` config
    _              -> putStrLn "Please provide a unique application name for your qmulus"

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
        void $ s3BucketLambda "copyS3Object" incoming (copyContentsLambda outgoing)

      copyContentsLambda
        :: S3BucketId
        -> S3Event
        -> LambdaProgram ()
      copyContentsLambda sinkBucketId event = do

        let incomingS3Obj = event ^. s3eObject
            outgoingS3Obj = s3oBucketId .~ sinkBucketId $ incomingS3Obj

        -- get the content of the newly uploaded file
        content <- getS3ObjectContent incomingS3Obj

        -- write the content into a new file in the "output" bucket
        putS3ObjectContent outgoingS3Obj content

