{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interface where

import           Control.Monad.Operational       (Program, singleton)
import           Data.Aeson                      (Value)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Conduit
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
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier            (DdbTableId)


type LambdaProgram = Program LambdaInstruction

type CompleteLambdaProgram = LambdaProgram LBS.ByteString

type ApiLambdaProgram = ApiMethodEvent  -> CompleteLambdaProgram
type S3LambdaProgram  = S3Event         -> CompleteLambdaProgram
type CfLambdaProgram  = CfEvent         -> CompleteLambdaProgram
type CwLambdaProgram  = CwEvent         -> CompleteLambdaProgram

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

  StreamFromS3Object
    :: S3Object
    -> (Sink BS.ByteString LambdaProgram a)
    -> LambdaInstruction a

  StreamS3Objects
    :: S3Object
    -> S3Object
    -> Conduit BS.ByteString LambdaProgram BS.ByteString
    -> LambdaInstruction ()

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

streamFromS3Object
  :: S3Object
  -> (Sink BS.ByteString LambdaProgram a)
  -> LambdaProgram a
streamFromS3Object s3Obj = singleton . StreamFromS3Object s3Obj

streamS3Objects
    :: S3Object
    -> S3Object
    -> Conduit BS.ByteString LambdaProgram BS.ByteString
    -> LambdaProgram ()
streamS3Objects inS3Obj outS3Obj = singleton . StreamS3Objects inS3Obj outS3Obj

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

