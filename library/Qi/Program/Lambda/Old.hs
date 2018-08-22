{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.Lambda.Old where

{-
import           Control.Monad.Freer
import           Data.Aeson                           (FromJSON, ToJSON, Value)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Conduit
import           Data.Text                            (Text)
import           Data.Time.Clock                      (UTCTime)
import           Network.AWS                          hiding (Request, Response)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan
import           Network.AWS.S3.Types                 (ETag)
import           Network.HTTP.Client
import           Protolude
import           Qi.AWS.Lex                           (BotSpec)
import           Qi.AWS.SQS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CfCustomResource.Types (CfCustomResourceEvent)
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.SQS
import           Qi.Config.Identifier
import           Qi.Core.Curry
import           Servant.Client                       (BaseUrl, ClientM,
                                                       ServantError)

type LambdaProgram                  = Program LambdaInstruction
type ApiLambdaProgram               = ApiMethodEvent        -> LambdaProgram LBS.ByteString
type S3LambdaProgram                = S3Event               -> LambdaProgram LBS.ByteString
type CfCustomResourceLambdaProgram  = CfCustomResourceEvent -> LambdaProgram LBS.ByteString
type CwLambdaProgram                = CwEvent               -> LambdaProgram LBS.ByteString
type DdbStreamLambdaProgram         = DdbStreamEvent        -> LambdaProgram LBS.ByteString

data LambdaInstruction a where


-- S3
  MultipartS3Upload
    :: S3Object
    -> (S3Object -> Text -> LambdaProgram [(Int, ETag)])
    -> LambdaInstruction ()

  UploadS3Chunk
    :: S3Object -- sink
    -> Text -- uploadId
    -> (Int, S3Object) -- source chunk
    -> LambdaInstruction (Maybe (Int, ETag))


  StreamFromS3Object
    :: S3Object
    -> (Sink BS.ByteString LambdaProgram a)
    -> LambdaInstruction a

  StreamS3Objects
    :: S3Object
    -> S3Object
    -> Conduit BS.ByteString LambdaProgram BS.ByteString
    -> LambdaInstruction ()


-- DDB

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


-- Lex

  StartBotImport
    :: BotSpec
    -> LambdaInstruction ()



-- S3


multipartS3Upload
  :: S3Object
  -> (S3Object -> Text -> LambdaProgram [(Int, ETag)])
  -> LambdaProgram ()
multipartS3Upload = singleton .: MultipartS3Upload

uploadS3Chunk
  :: S3Object -- sink
  -> Text -- uploadId
  -> (Int, S3Object) -- source chunk
  -> LambdaProgram (Maybe (Int, ETag))
uploadS3Chunk = singleton .:: UploadS3Chunk


streamFromS3Object
  :: S3Object
  -> (Sink BS.ByteString LambdaProgram a)
  -> LambdaProgram a
streamFromS3Object = singleton .: StreamFromS3Object

streamS3Objects
    :: S3Object
    -> S3Object
    -> Conduit BS.ByteString LambdaProgram BS.ByteString
    -> LambdaProgram ()
streamS3Objects = singleton .:: StreamS3Objects



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
queryDdbRecords = singleton .:: QueryDdbRecords

getDdbRecord
  :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram GetItemResponse
getDdbRecord = singleton .: GetDdbRecord

putDdbRecord :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram PutItemResponse
putDdbRecord = singleton .: PutDdbRecord

deleteDdbRecord
  :: DdbTableId
  -> DdbAttrs
  -> LambdaProgram DeleteItemResponse
deleteDdbRecord = singleton .: DeleteDdbRecord


-- Lex

startBotImport
  :: BotSpec
  -> LambdaProgram ()
startBotImport = singleton . StartBotImport

-}
