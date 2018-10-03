{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad.Freer
import           Data.Default           (def)
import           Protolude
import           Qi                     (withConfig)
import           Qi.Config.AWS.Lambda   (LambdaMemorySize (..), lpMemorySize)
import           Qi.Config.AWS.S3       (S3Event, s3Object, s3eObject,
                                         s3oBucketId, s3oKey)
import           Qi.Config.Identifier   (S3BucketId)
import           Qi.Program.Config.Lang (ConfigEff, s3Bucket, s3BucketLambda)
import           Qi.Program.Gen.Lang    (GenEff, say)
import           Qi.Program.S3.Lang     (S3Eff, S3LambdaProgram, getContent,
                                         putContent)


main :: IO ()
main = withConfig config
  where
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
      :: (Member S3Eff effs, Member GenEff effs)
      => S3BucketId
      -> S3LambdaProgram effs
    copyContentsLambda sinkBucketId = lbd
      where
        lbd event = do
          let incomingS3Obj = event ^. s3eObject
              outgoingS3Obj = s3oBucketId .~ sinkBucketId $ incomingS3Obj

          say "getting content"
          -- get the content of the newly uploaded file
          eitherContent <- getContent incomingS3Obj

          case eitherContent of
            Right content -> do
              say "putting content"
              -- write the content into a new file in the "output" bucket
              putContent outgoingS3Obj content

              pure "lambda had executed successfully"

            Left err ->
              pure . toS $ "error: '" <> err <> "'"

