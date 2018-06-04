{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interface where

import           Control.Monad.Operational       (Program, singleton)
import           Data.Aeson                      (FromJSON, ToJSON, Value)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.Conduit
import           Data.Text                       (Text)
import           Data.Time.Clock                 (UTCTime)
import           Network.AWS                     hiding (Request, Response)
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan
import           Network.HTTP.Client
import           Protolude
import           Qi.AWS.SQS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.SQS
import           Qi.Config.Identifier
import           Qi.Core.Curry
import           Servant.Client                  (BaseUrl, ClientM,
                                                  ServantError)


type LambdaProgram          = Program LambdaInstruction
type CompleteLambdaProgram  = LambdaProgram LBS.ByteString
type GenericLambdaProgram   = Value           -> CompleteLambdaProgram
type ApiLambdaProgram       = ApiMethodEvent  -> CompleteLambdaProgram
type S3LambdaProgram        = S3Event         -> CompleteLambdaProgram
type CfLambdaProgram        = CfEvent         -> CompleteLambdaProgram
type CwLambdaProgram        = CwEvent         -> CompleteLambdaProgram
type DdbStreamLambdaProgram = DdbStreamEvent  -> CompleteLambdaProgram

data LambdaInstruction a where

  GetAppName
    :: LambdaInstruction Text

  Http
    :: ManagerSettings
    -> Request
    -> LambdaInstruction (Response LBS.ByteString)

  RunServant
    :: ManagerSettings
    -> BaseUrl
    -> ClientM a
    -> LambdaInstruction (Either ServantError a)

  AmazonkaSend
    :: (AWSRequest a)
    => a
    -> LambdaInstruction (Rs a)

  GetS3ObjectContent
    :: S3Object
    -> LambdaInstruction LBS.ByteString
{-
  StreamFromS3Object
    :: S3Object
    -> (Sink BS.ByteString LambdaProgram a)
    -> LambdaInstruction a

  StreamS3Objects
    :: S3Object
    -> S3Object
    -> Conduit BS.ByteString LambdaProgram BS.ByteString
    -> LambdaInstruction ()
-}

  PutS3ObjectContent
    :: S3Object
    -> LBS.ByteString
    -> LambdaInstruction ()

  ListS3Objects
    :: S3BucketId
    -> LambdaInstruction [S3Object]

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


-- SQS

  SendMessage
    :: ToJSON a
    => SqsQueueId
    -> a
    -> LambdaInstruction ()

  ReceiveMessage
    :: FromJSON a
    => SqsQueueId
    -> LambdaInstruction [(a, ReceiptHandle)] -- the json body and the receipt handle

  DeleteMessage
    :: SqsQueueId
    -> ReceiptHandle
    -> LambdaInstruction ()



  Say
    :: Text
    -> LambdaInstruction ()

  Sleep
    :: Int
    -> LambdaInstruction ()

  GetCurrentTime
    :: LambdaInstruction UTCTime



-- HTTP client

http
  :: ManagerSettings
  -> Request
  -> LambdaProgram (Response LBS.ByteString)
http =
  singleton .: Http

-- Servant

runServant
  :: ManagerSettings
  -> BaseUrl
  -> ClientM a
  -> LambdaProgram (Either ServantError a)
runServant =
  singleton .:: RunServant


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
{-
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
-}
putS3ObjectContent
  :: S3Object
  -> LBS.ByteString
  -> LambdaProgram ()
putS3ObjectContent = singleton .: PutS3ObjectContent

listS3Objects
  :: S3BucketId
  -> LambdaProgram [S3Object]
listS3Objects = singleton . ListS3Objects


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


-- SQS

sendMessage
  :: ToJSON a
  => SqsQueueId
  -> a
  -> LambdaProgram ()
sendMessage = singleton .: SendMessage

receiveMessage
  :: FromJSON a
  => SqsQueueId
  -> LambdaProgram [(a, ReceiptHandle)] -- the json body and the receipt handle
receiveMessage = singleton . ReceiveMessage

deleteMessage
  :: SqsQueueId
  -> ReceiptHandle
  -> LambdaProgram ()
deleteMessage = singleton .: DeleteMessage


-- Util

getAppName
  :: LambdaProgram Text
getAppName =
  singleton GetAppName

getCurrentTime
  :: LambdaProgram UTCTime
getCurrentTime = singleton GetCurrentTime

sleep
  :: Int
  -> LambdaProgram ()
sleep = singleton . Sleep

say
  :: Text
  -> LambdaProgram ()
say = singleton . Say

