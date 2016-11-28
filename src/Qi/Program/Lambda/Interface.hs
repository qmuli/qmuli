{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interface where

import           Control.Monad.Operational       (Program, singleton)
import           Data.Aeson                      (Value)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Text                       (Text)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
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

  QueryDdbRecords
    :: DdbTableId
    -> Maybe Text
    -> LambdaInstruction QueryResponse

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

getS3ObjectContent :: S3Object -> LambdaProgram LBS.ByteString
getS3ObjectContent = singleton . GetS3ObjectContent

putS3ObjectContent :: S3Object -> LBS.ByteString -> LambdaProgram ()
putS3ObjectContent s3Obj = singleton . PutS3ObjectContent s3Obj


-- DDB

scanDdbRecords :: DdbTableId -> LambdaProgram ScanResponse
scanDdbRecords = singleton . ScanDdbRecords

queryDdbRecords :: DdbTableId -> Maybe Text -> LambdaProgram QueryResponse
queryDdbRecords ddbTableId = singleton . QueryDdbRecords ddbTableId

getDdbRecord :: DdbTableId -> DdbAttrs -> LambdaProgram GetItemResponse
getDdbRecord ddbTableId = singleton . GetDdbRecord ddbTableId

putDdbRecord :: DdbTableId -> DdbAttrs -> LambdaProgram PutItemResponse
putDdbRecord ddbTableId = singleton . PutDdbRecord ddbTableId

deleteDdbRecord :: DdbTableId -> DdbAttrs -> LambdaProgram DeleteItemResponse
deleteDdbRecord ddbTableId = singleton . DeleteDdbRecord ddbTableId

-- Util

respond :: Int -> Value -> LambdaProgram ()
respond status = singleton . Respond status
