{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad               (void)
import qualified Data.ByteString.Char8       as BS
import qualified Data.Conduit.List           as CL
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
import           Qi.Program.Lambda.Interface (S3LambdaProgram, say,
                                              streamFromS3Object)
import           Qi.Util                     (success)


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      incoming <- s3Bucket "incoming"

      let lbdProfile = def
                        & lpMemorySize .~ M1024
                        & lpTimeoutSeconds .~ 300

      void $ s3BucketLambda "foldS3ObjectContent" incoming foldS3ObjectContent lbdProfile

    foldS3ObjectContent
      :: S3LambdaProgram
    foldS3ObjectContent event = do
      let incomingS3Obj = event^.s3eObject

      -- Calculate the size of the stream in constant memory space (i.e. without downloading the whole s3 object into memory)
      (size :: Int) <- streamFromS3Object incomingS3Obj $
        CL.foldM (\acc bs -> do
                    let len = BS.length bs
                    say . T.pack $ "encoundered chunk size: " ++ show len
                    return $ acc + len
                  ) 0

      say . T.pack $ "s3 object content size: " ++ show size
      success "lambda had executed successfully"

