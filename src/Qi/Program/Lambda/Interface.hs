{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interface where

import           Control.Monad.Operational       (Program, singleton)
import           Data.Aeson                      (Value)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Scan
import           Qi.Config.Identifier            (DdbTableId)

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

  ScanDdbRecords
    :: DdbTableId
    -> LambdaInstruction ScanResponse

  GetDdbRecord
    :: DdbTableId
    -> DdbAttrs
    -> LambdaInstruction GetItemResponse

  PutDdbRecord
    :: DdbTableId
    -> DdbAttrs
    -> LambdaInstruction PutItemResponse

  DeleteDdbRecord
    :: DdbTableId
    -> DdbAttrs
    -> LambdaInstruction DeleteItemResponse

  Respond
    :: Int
    -> Value
    -> LambdaInstruction ()


-- S3

getS3ObjectContent = singleton . GetS3ObjectContent

putS3ObjectContent s3Obj = singleton . PutS3ObjectContent s3Obj


-- DDB

scanDdbRecords = singleton . ScanDdbRecords

getDdbRecord ddbTableId = singleton . GetDdbRecord ddbTableId

putDdbRecord ddbTableId = singleton . PutDdbRecord ddbTableId

deleteDdbRecord ddbTableId = singleton . DeleteDdbRecord ddbTableId

-- Util

respond status = singleton . Respond status
