{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interface where

import           Control.Monad.Operational
import           Data.ByteString           (ByteString)

import           Qi.Config.AWS.S3


type LambdaProgram a = Program LambdaInstruction a

data LambdaInstruction a where
  GetS3ObjectContent :: S3Object -> LambdaInstruction ByteString
  PutS3ObjectContent :: S3Object -> ByteString -> LambdaInstruction ()


getS3ObjectContent :: S3Object -> LambdaProgram ByteString
getS3ObjectContent = singleton . GetS3ObjectContent

putS3ObjectContent :: S3Object -> ByteString -> LambdaProgram ()
putS3ObjectContent s3Obj = singleton . PutS3ObjectContent s3Obj


