{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interpreters.IO where

import           Control.Lens
import           Control.Monad.Operational
import           Control.Monad.Trans.Class   (lift)
import           Data.ByteString             (ByteString)
import           Data.Default                (def)
import           Data.Hashable               (hash)

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (LambdaInstruction (..),
                                              LambdaProgram)


interpret
  :: LambdaProgram ()
  -> IO ()
interpret lambda =  do
  i <- viewT lambda
  case i of
    (GetS3ObjectContent s3Obj) :>>= is -> do
      interpret . is =<< getS3ObjectContent s3Obj

    (PutS3ObjectContent s3Obj content) :>>= is -> do
      interpret . is =<< putS3ObjectContent s3Obj content

    Return _ ->
      return def


  where

    getS3ObjectContent
      :: S3Object
      -> IO ByteString
    getS3ObjectContent s3Obj = do
      putStrLn $ "GetS3ObjectContent: " ++ show s3Obj
      return "test output"

    putS3ObjectContent
      :: S3Object
      -> ByteString
      -> IO ()
    putS3ObjectContent s3Obj content = do
      putStrLn $ "PutS3ObjectContent: " ++ show s3Obj ++ " " ++ show content

