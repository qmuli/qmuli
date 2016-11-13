{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interface where

import           Control.Monad.Operational    (Program, singleton)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Text                    (Text)
import           Qi.Config.Identifier         (DdbTableId)

import           Qi.Config.AWS.Api
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.S3


type LambdaProgram a = Program LambdaInstruction a

instance Show (ApiEvent -> LambdaProgram ()) where
  show _ = "..."

instance Show (S3Event -> LambdaProgram ()) where
  show _ = "..."

data LambdaInstruction a where
  GetS3ObjectContent
    :: S3Object
    -> LambdaInstruction LBS.ByteString

  PutS3ObjectContent
    :: S3Object
    -> LBS.ByteString
    -> LambdaInstruction ()

  GetDdbRecord
    :: DdbTableId
    -> DdbAttrs
    -> LambdaInstruction DdbAttrs

  PutDdbRecord
    :: DdbTableId
    -> DdbAttrs
    -> LambdaInstruction ()

  Output
    :: BS.ByteString
    -> LambdaInstruction ()


getS3ObjectContent
  :: S3Object
  -> LambdaProgram LBS.ByteString
getS3ObjectContent = singleton . GetS3ObjectContent

putS3ObjectContent
  :: S3Object
  -> LBS.ByteString
  -> LambdaProgram ()
putS3ObjectContent s3Obj = singleton . PutS3ObjectContent s3Obj

getDdbRecord
  :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram DdbAttrs
getDdbRecord ddbTableId = singleton . GetDdbRecord ddbTableId

putDdbRecord
  :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram ()
putDdbRecord ddbTableId = singleton . PutDdbRecord ddbTableId

output
  :: BS.ByteString
  -> LambdaProgram ()
output = singleton . Output
