{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interface where

import           Control.Monad.Operational       (Program, singleton)
import           Data.Aeson                      (Value)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Text                       (Text)
import           Network.AWS                     hiding (Request, Response)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan
import           Network.HTTP.Client

import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier            (DdbTableId)


type LambdaProgram a = Program LambdaInstruction a

data LambdaInstruction a where

  Http
    :: Request
    -> ManagerSettings
    -> LambdaInstruction (Response LBS.ByteString)

  AmazonkaSend
    :: (AWSRequest a)
    => a
    -> LambdaInstruction (Rs a)

  GetS3ObjectContent
    :: S3Object
    -> LambdaInstruction LBS.ByteString

  FoldStreamFromS3Object
    :: S3Object
    -> (a -> ByteString -> a)
    -> a
    -> LambdaInstruction a

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
    -> DdbAttrs
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

  Say
    :: Text
    -> LambdaInstruction ()

  Output
    :: LBS.ByteString
    -> LambdaInstruction ()


-- HTTP client

http
  :: Request
  -> ManagerSettings
  -> LambdaProgram (Response LBS.ByteString)
http request =
  singleton . Http request

-- Amazonka

amazonkaSend
  :: (AWSRequest a)
  => a
  -> LambdaProgram (Rs a)
amazonkaSend = singleton . AmazonkaSend


-- S3

getS3ObjectContent
  :: S3Object
  -> LambdaProgram LBS.ByteString
getS3ObjectContent = singleton . GetS3ObjectContent

foldStreamFromS3Object
  :: S3Object
  -> (a -> ByteString -> a)
  -> a
  -> LambdaProgram a
foldStreamFromS3Object s3Obj folder = singleton . FoldStreamFromS3Object s3Obj folder

putS3ObjectContent
  :: S3Object
  -> LBS.ByteString
  -> LambdaProgram ()
putS3ObjectContent s3Obj = singleton . PutS3ObjectContent s3Obj


-- DDB

scanDdbRecords
  :: DdbTableId
  -> LambdaProgram ScanResponse
scanDdbRecords = singleton . ScanDdbRecords

queryDdbRecords
  :: DdbTableId
  -> Maybe Text
  -> DdbAttrs
  -> LambdaProgram QueryResponse
queryDdbRecords ddbTableId keyCond = singleton . QueryDdbRecords ddbTableId keyCond

getDdbRecord
  :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram GetItemResponse
getDdbRecord ddbTableId = singleton . GetDdbRecord ddbTableId

putDdbRecord :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram PutItemResponse
putDdbRecord ddbTableId = singleton . PutDdbRecord ddbTableId

deleteDdbRecord
  :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram DeleteItemResponse
deleteDdbRecord ddbTableId = singleton . DeleteDdbRecord ddbTableId

-- Util

say
  :: Text
  -> LambdaProgram ()
say = singleton . Say

output
  :: LBS.ByteString
  -> LambdaProgram ()
output = singleton . Output
