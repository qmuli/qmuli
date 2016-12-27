{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import           Control.Monad.Identity      (Identity)
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Conduit                (Conduit, awaitForever, yield)
import           Data.Default                (def)
import qualified Data.Text                   as T

import           Qi                          (withConfig)
import           Qi.Config.AWS.Lambda        (LambdaMemorySize (..),
                                              lpMemorySize, lpTimeoutSeconds)
import           Qi.Config.AWS.S3            (S3Event, s3Object, s3eObject,
                                              s3oBucketId, s3oKey)
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram, s3Bucket,
                                              s3BucketLambda)
import           Qi.Program.Lambda.Interface (LambdaProgram, S3LambdaProgram,
                                              say, streamS3Objects)
import           Qi.Util                     (success)


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      -- create an "input" s3 bucket
      incoming <- s3Bucket "incoming"

      -- create an "output" s3 bucket
      outgoing <- s3Bucket "outgoing"

      let lbdProfile = def
                        & lpMemorySize .~ M1536
                        & lpTimeoutSeconds .~ 300

      void $ s3BucketLambda "streamS3Objects" incoming (streamS3ObjectsLambda outgoing) lbdProfile

    streamS3ObjectsLambda
      :: S3BucketId
      -> S3LambdaProgram
    streamS3ObjectsLambda sinkBucketId event = do
      let incomingS3Obj = event ^. s3eObject
          outgoingS3Obj = s3oBucketId .~ sinkBucketId $ incomingS3Obj

          conduit :: Conduit BS.ByteString LambdaProgram BS.ByteString
          conduit = awaitForever $ \bs -> do
                      lift . say . T.pack $ "streaming chunk of size: " ++ show (BS.length bs)
                      yield bs

      void $ streamS3Objects incomingS3Obj outgoingS3Obj conduit

      success "lambda had executed successfully"
