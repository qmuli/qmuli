{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad                (void)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.Default                 (def)

import           Qi                           (withConfig)
import           Qi.Config.AWS.Lambda.Profile (LambdaMemorySize (..),
                                               lpMemorySize, lpTimeoutSeconds)
import           Qi.Config.AWS.S3             (S3Event, s3Object, s3eObject,
                                               s3oBucketId, s3oKey)
import           Qi.Config.Identifier         (S3BucketId)
import           Qi.Program.Config.Interface  (ConfigProgram, s3Bucket,
                                               s3BucketLambda)
import           Qi.Program.Lambda.Interface  (LambdaProgram,
                                               foldStreamFromS3Object, output)


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      incoming <- s3Bucket "incoming"

      let lbdProfile = def
                        & lpMemorySize .~ M128
                        & lpTimeoutSeconds .~ 300

      void $ s3BucketLambda "foldS3ObjectContent" incoming foldS3ObjectContent lbdProfile

    foldS3ObjectContent
      :: S3Event
      -> LambdaProgram ()
    foldS3ObjectContent event = do
      let incomingS3Obj = event^.s3eObject

      -- Calculate the size of the stream in constant memory space (i.e. without downloading the whole s3 object into memory)
      (size :: Int) <- foldStreamFromS3Object incomingS3Obj (\acc bs -> acc + BS.length bs) 0

      output . LBS.pack $ "content size: " ++ show size
