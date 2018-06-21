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
import           Network.AWS.S3.Types            (ETag)
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

type LambdaProgram            = Program LambdaInstruction
type ApiLambdaProgram         = ApiMethodEvent  -> LambdaProgram LBS.ByteString
type S3LambdaProgram          = S3Event         -> LambdaProgram LBS.ByteString
type CfLambdaProgram          = CfEvent         -> LambdaProgram LBS.ByteString
type CwLambdaProgram          = CwEvent         -> LambdaProgram LBS.ByteString
type DdbStreamLambdaProgram   = DdbStreamEvent  -> LambdaProgram LBS.ByteString

data LambdaInstruction a where

  GetAppName
    :: LambdaInstruction Text

  RunServant
    :: ManagerSettings
    -> BaseUrl
    -> ClientM a
    -> LambdaInstruction (Either ServantError a)

  AmazonkaSend
    :: (AWSRequest a)
    => a
    -> LambdaInstruction (Rs a)

  InvokeLambda
    :: ToJSON a
    => LambdaId
    -> a
    -> LambdaInstruction ()

  GetS3ObjectContent
    :: S3Object
    -> LambdaInstruction (Either Text LBS.ByteString)

  MultipartS3Upload
    :: S3Object
    -> (S3Object -> Text -> LambdaProgram [(Int, ETag)])
    -> LambdaInstruction ()

  UploadS3Chunk
    :: S3Object -- sink
    -> Text -- uploadId
    -> (Int, S3Object) -- source chunk
    -> LambdaInstruction (Maybe (Int, ETag))


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
    :: Monoid a
    => S3BucketId
    -> (a -> [S3Object] -> LambdaProgram a)
    -> LambdaInstruction a

  DeleteS3Object
    :: S3Object
    -> LambdaInstruction ()

  DeleteS3Objects
    :: [S3Object]
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


-- Lambda

invokeLambda
  :: ToJSON a
  => LambdaId
  -> a
  -> LambdaProgram ()
invokeLambda = singleton .: InvokeLambda


-- S3

getS3ObjectContent
  :: S3Object
  -> LambdaProgram (Either Text LBS.ByteString)
getS3ObjectContent = singleton . GetS3ObjectContent


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
  :: Monoid a
  => S3BucketId
  -> (a -> [S3Object] -> LambdaProgram a)
  -> LambdaProgram a
listS3Objects = singleton .: ListS3Objects

deleteS3Object
  :: S3Object
  -> LambdaProgram ()
deleteS3Object = singleton . DeleteS3Object

deleteS3Objects
  :: [S3Object]
  -> LambdaProgram ()
deleteS3Objects = singleton . DeleteS3Objects


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

