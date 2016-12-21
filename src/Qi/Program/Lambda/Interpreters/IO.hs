{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module Qi.Program.Lambda.Interpreters.IO (run) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens                 hiding (view)
import           Control.Monad                (void)
import           Control.Monad.Base           (MonadBase)
import           Control.Monad.Catch          (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Operational    (ProgramViewT ((:>>=), Return),
                                               singleton, view)
import           Control.Monad.Reader.Class   (MonadReader)
import           Control.Monad.Trans.AWS      (AWST, runAWST, send)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
import           Data.Aeson                   (Value (..), encode, object)
import           Data.Binary.Builder          (fromLazyByteString,
                                               toLazyByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   (ByteString)
import qualified Data.ByteString.Lazy.Char8   as LBS
import qualified Data.Conduit                 as C
import           Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit.List            as CL
import           Data.Default                 (def)
import           Data.Monoid                  ((<>))
import           Data.Ratio                   (numerator)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8)
import qualified Data.Text.IO                 as T
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Network.AWS                  hiding (Request, Response, send)
import           Network.AWS.CloudWatchLogs
import           Network.AWS.Data.Body        (fuseStream)
import           Network.AWS.DynamoDB
import           Network.AWS.S3
import           Network.HTTP.Client          (ManagerSettings, Request,
                                               Response, httpLbs, newManager)

import           Qi.Amazonka                  (currentRegion)
import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.DDB.Accessors
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors
import           Qi.Config.Identifier         (DdbTableId)
import           Qi.Program.Lambda.Interface  (LambdaInstruction (..),
                                               LambdaProgram)
import           System.IO                    (stdout)


-- This would have been nice, but the monad stack needs another ReaderT and one
-- seemingly cannot flatten two ReaderT-s in a similar fashion
{- newtype QiAWS a = QiAWS {unQiAWS :: AWST (ResourceT IO) a} -}
  {- deriving ( -}
      {- Functor -}
    {- , Applicative -}
    {- , Monad -}
    {- , MonadIO -}
    {- , MonadCatch -}
    {- , MonadThrow -}
    {- , MonadResource -}
    {- , MonadBase IO -}
    {- , MonadReader Env -}
    {- , MonadAWS -}
    {- ) -}

type QiAWS a = AWST (ReaderT (Text, Config) (ResourceT IO)) a



cloudWatchLoggerWorker
  :: Text
  -> Config
  -> MVar ()
  -> TChan (Maybe Text)
  -> IO ()
cloudWatchLoggerWorker lbdName config done chan = do
  noLoggerEnv <- newEnv Discover <&> set envRegion currentRegion

  runResourceT . runAWST noLoggerEnv $ do

    clgr <- send $ createLogGroup groupName
    -- ignore response
    clsr <- send $ createLogStream groupName streamName
    -- ignore response

    loop Nothing

    where
      groupName = config^.namePrefix
      streamName = lbdName

      loop
        :: Maybe Text
        -> AWS ()
      loop nextToken = do
        maybeMsg <- liftIO $ atomically (readTChan chan)
        case maybeMsg of
          Just msg -> do
            -- get current UTC epoch time in milliseconds
            ts <- liftIO $ fromIntegral . round . (* 1000) <$> getPOSIXTime

            let logEvent = inputLogEvent ts msg

            r <- send $ putLogEvents groupName streamName [logEvent]
                            & pleSequenceToken .~ nextToken

            loop $ r^.plersNextSequenceToken

          Nothing ->
            liftIO $ putMVar done ()



run
  :: Text
  -> Config
  -> LambdaProgram ()
  -> IO ()
run lbdName config program = do
  loggerDone <- newEmptyMVar
  logChan <- liftIO $ atomically newTChan
  liftIO $ forkIO $ cloudWatchLoggerWorker lbdName config loggerDone logChan

  let cloudWatchLogger :: Logger
      cloudWatchLogger level bd = do
        let msg = fromLazyByteString (LBS.fromChunks ["[lambda][amazonka][]", "[", BS.pack $ show level, "]"]) <> bd
        atomically . writeTChan logChan . Just . decodeUtf8 . LBS.toStrict $ toLazyByteString msg

  cloudWatchLoggerEnv <- newEnv Discover <&> set envRegion currentRegion . set envLogger cloudWatchLogger

  runResourceT . (`runReaderT` (lbdName, config)) . runAWST cloudWatchLoggerEnv $ go logChan program

  atomically $ writeTChan logChan Nothing -- signal the end
  takeMVar loggerDone -- wait on the logger to finish logging messages

  where
    go
      :: TChan (Maybe Text)
      -> LambdaProgram ()
      -> QiAWS ()
    go logChan = interpret
      where
        logMessage
          :: Text
          -> QiAWS ()
        logMessage = liftIO . atomically . writeTChan logChan . Just . T.append "[Message]"

        interpret
          :: LambdaProgram ()
          -> QiAWS ()
        interpret program =
          case view program of

-- Http
            (Http request ms) :>>= is ->
              interpret . is =<< http request ms

-- Amazonka
            (AmazonkaSend cmd) :>>= is ->
              interpret . is =<< amazonkaSend cmd

-- S3
            (GetS3ObjectContent s3Obj) :>>= is ->
              interpret . is =<< getS3ObjectContent s3Obj

            (FoldStreamFromS3Object s3Obj folder zero) :>>= is ->
              interpret . is =<< foldStreamFromS3Object s3Obj folder zero

            (PutS3ObjectContent s3Obj content) :>>= is ->
              interpret . is =<< putS3ObjectContent s3Obj content

-- DDB
            (ScanDdbRecords ddbTableId) :>>= is ->
              interpret . is =<< scanDdbRecords ddbTableId

            (QueryDdbRecords ddbTableId keyCond expAttrs) :>>= is ->
              interpret . is =<< queryDdbRecords ddbTableId keyCond expAttrs

            (GetDdbRecord ddbTableId keys) :>>= is ->
              interpret . is =<< getDdbRecord ddbTableId keys

            (PutDdbRecord ddbTableId item) :>>= is ->
              interpret . is =<< putDdbRecord ddbTableId item

            (DeleteDdbRecord ddbTableId key) :>>= is ->
              interpret . is =<< deleteDdbRecord ddbTableId key

-- Util
            (Say text) :>>= is ->
              interpret . is =<< say text

            (Output content) :>>= is ->
              output content -- final output, no more program instructions

            Return _ -> do
              -- if it gets this far, just return success to keep the js shim happy
              output . encode $ object [
                  ("status", Number 200)
                , ("body", object [("message", "lambda had executed successfully")])
                ]


-- Http

        http
          :: Request
          -> ManagerSettings
          -> QiAWS (Response ByteString)
        http request ms = liftIO $ do
          manager <- newManager ms
          httpLbs request manager

-- Amazonka
        amazonkaSend
          :: (AWSRequest a)
          => a
          -> QiAWS (Rs a)
        amazonkaSend = send

-- S3
        getS3ObjectContent
          :: S3Object
          -> QiAWS ByteString
        getS3ObjectContent S3Object{_s3oBucketId, _s3oKey = S3Key objKey} = do
          let bucketName = getFullBucketName bucket config
              bucket = getS3BucketById _s3oBucketId config

          r <- send . getObject (BucketName bucketName) $ ObjectKey objKey
          sinkBody (r ^. gorsBody) sinkLbs

        foldStreamFromS3Object
          :: S3Object
          -> (a -> BS.ByteString -> a)
          -> a
          -> QiAWS a
        foldStreamFromS3Object S3Object{_s3oBucketId, _s3oKey = S3Key objKey} folder zero = do
          let bucketName = getFullBucketName bucket config
              bucket = getS3BucketById _s3oBucketId config

          r <- send . getObject (BucketName bucketName) $ ObjectKey objKey
          sinkBody (r ^. gorsBody) $ CL.fold folder zero


        -- TODO: come up with a way to stream one S3 object into another one while transforming it in some way

        {- streamS3Objects -}
          {- :: S3Object -}
          {- -> S3Object -}
          {- -> (BS.ByteString -> BS.ByteString) -}
          {- -> QiAWS () -}
        {- streamS3Objects -}
          {- fromS3Obj@S3Object{_s3oKey = S3Key fromObjKey} -}
          {- toS3Obj@S3Object{_s3oKey = S3Key toObjKey} -}
          {- f = do -}
            {- let fromBucketName = getFullBucketName fromBucket config -}
                {- fromBucket = getS3BucketById (fromS3Obj^.s3oBucketId) config -}

                {- toBucketName = getFullBucketName toBucket config -}
                {- toBucket = getS3BucketById (toS3Obj^.s3oBucketId) config -}

            {- rr <- send . getObject (BucketName fromBucketName) $ ObjectKey fromObjKey -}

            {- let xformedBody = fuseStream (rr ^. gorsBody) $ CL.map f -}
                {- src = xformedBody^.streamBody -}


            {- wr <- send . putObject (BucketName toBucketName) (ObjectKey toObjKey) $ toBody xformedBody -}

            {- return () -}


        putS3ObjectContent
          :: S3Object
          -> ByteString
          -> QiAWS ()
        putS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key objKey} content = do
          r <- send . putObject (BucketName bucketName) (ObjectKey objKey) $ toBody content
          return ()

          where
            bucketName = getFullBucketName bucket config
            bucket = getS3BucketById _s3oBucketId config


-- DynamoDB
        scanDdbRecords
          :: DdbTableId
          -> QiAWS ScanResponse
        scanDdbRecords ddbTableId = do
          send $ scan tableName

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        queryDdbRecords
          :: DdbTableId
          -> Maybe Text
          -> DdbAttrs
          -> QiAWS QueryResponse
        queryDdbRecords ddbTableId keyCond expAttrs = do
          send $ query tableName
            & qKeyConditionExpression .~ keyCond
            & qExpressionAttributeValues .~ expAttrs

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        getDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS GetItemResponse
        getDdbRecord ddbTableId keys = do
          send $ getItem tableName & giKey .~ keys

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        putDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS PutItemResponse
        putDdbRecord ddbTableId item = do
          send $ putItem tableName & piItem .~ item

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        deleteDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS DeleteItemResponse
        deleteDdbRecord ddbTableId key = do
          send $ deleteItem tableName & diKey .~ key

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


-- Util
        say
          :: Text
          -> QiAWS ()
        say = logMessage


        output
          :: ByteString
          -> QiAWS ()
        output content =
          liftIO $ LBS.putStr content




