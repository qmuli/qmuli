{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Qi.Program.Lambda.Interpreters.IO (run) where

import           Control.Concurrent                    hiding (yield)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens                          hiding (view)
import           Control.Monad                         (void, (<=<))
import           Control.Monad.Base                    (MonadBase)
import           Control.Monad.Catch                   (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Operational             (ProgramViewT ((:>>=), Return),
                                                        singleton, view)
import           Control.Monad.Reader.Class            (MonadReader)
import           Control.Monad.Trans.AWS               (AWST, runAWST, send)
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Reader            (ReaderT, ask, asks,
                                                        runReaderT)
import           Control.Monad.Trans.Resource          (MonadResource,
                                                        ResourceT)
import           Data.Aeson                            (Value (..), encode,
                                                        object)
import           Data.Binary.Builder                   (fromLazyByteString,
                                                        toLazyByteString)
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy.Char8            as LBS
import           Data.Conduit                          (Conduit, Sink,
                                                        awaitForever, transPipe,
                                                        unwrapResumable, yield,
                                                        ($$), (=$=))
import           Data.Conduit.Binary                   (sinkLbs)
import qualified Data.Conduit.List                     as CL
import           Data.Default                          (def)
import           Data.Monoid                           ((<>))
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (decodeUtf8)
import qualified Data.Text.IO                          as T
import           GHC.Exts                              (fromList)
import           Network.AWS                           hiding (Request,
                                                        Response, send)
import           Network.AWS.CloudWatchLogs
import           Network.AWS.Data.Body                 (RsBody (..), fuseStream)
import           Network.AWS.DynamoDB
import           Network.AWS.S3
import           Network.AWS.S3.CreateMultipartUpload
import           Network.AWS.S3.StreamingUpload
import           Network.HTTP.Client                   (ManagerSettings,
                                                        Request, Response,
                                                        httpLbs, newManager)
import           System.Mem                            (performMajorGC)

import           Qi.Amazonka                           (currentRegion)
import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier                  (DdbTableId)
import           Qi.Program.Lambda.Interface           (LambdaInstruction (..),
                                                        LambdaProgram)
import           Qi.Program.Lambda.Interpreters.IO.Log


newtype QiAWS a = QiAWS {unQiAWS :: AWST (ResourceT IO) a}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadResource
    , MonadBase IO
    , MonadReader Env
    , MonadAWS
    )

liftAWSFromResourceIO = liftAWS . lift


run
  :: Text
  -> Config
  -> LambdaProgram LBS.ByteString
  -> IO ()
run lbdName config program = do
  logChan     <- liftIO $ atomically newTChan
  loggerDone  <- liftIO . forkIOSync $ cloudWatchLoggerWorker lbdName config logChan

  let cloudWatchLogger :: Logger
      cloudWatchLogger level bd = do
        let prefix = LBS.fromChunks ["[lambda][amazonka][]", "[", BS.pack $ show level, "] "]
            msg = fromLazyByteString prefix <> bd
        queueLogEntry logChan . decodeUtf8 . LBS.toStrict $ toLazyByteString msg

  cloudWatchLoggerEnv <- newEnv Discover <&> set envRegion currentRegion . set envLogger cloudWatchLogger

  runResourceT . runAWST cloudWatchLoggerEnv . unQiAWS $ do
    output <- go logChan program
    liftIO $ LBS.putStr output

  signalLogEnd logChan
  takeMVar loggerDone -- wait on the logger to finish logging messages

  where
    go
      :: LogChan
      -> LambdaProgram a
      -> QiAWS a
    go logChan = interpret
      where
        logMessage
          :: Text
          -> QiAWS ()
        logMessage msg = liftIO . queueLogEntry logChan $ T.append "[Message] " msg

        interpret
          :: LambdaProgram a
          -> QiAWS a
        interpret program =
          case view program of

-- Http
            (Http request ms) :>>= is ->
               http request ms >>= interpret . is

-- Amazonka
            (AmazonkaSend cmd) :>>= is ->
              amazonkaSend cmd >>= interpret . is

-- S3
            (GetS3ObjectContent s3Obj) :>>= is ->
              getS3ObjectContent s3Obj >>= interpret . is

            (StreamFromS3Object s3Obj sink) :>>= is ->
              streamFromS3Object s3Obj sink >>= interpret . is

            (StreamS3Objects inS3Obj outS3Obj conduit) :>>= is ->
              streamS3Objects inS3Obj outS3Obj conduit >>= interpret . is

            (PutS3ObjectContent s3Obj content) :>>= is ->
              putS3ObjectContent s3Obj content >>= interpret . is

-- DDB
            (ScanDdbRecords ddbTableId) :>>= is ->
              scanDdbRecords ddbTableId >>= interpret . is

            (QueryDdbRecords ddbTableId keyCond expAttrs) :>>= is ->
              queryDdbRecords ddbTableId keyCond expAttrs >>= interpret . is

            (GetDdbRecord ddbTableId keys) :>>= is ->
              getDdbRecord ddbTableId keys >>= interpret . is

            (PutDdbRecord ddbTableId item) :>>= is ->
              putDdbRecord ddbTableId item >>= interpret . is

            (DeleteDdbRecord ddbTableId key) :>>= is ->
              deleteDdbRecord ddbTableId key >>= interpret . is

-- Util
            (Say text) :>>= is ->
              say text >>= interpret . is

            Return x ->
              return x


-- Http

        http
          :: Request
          -> ManagerSettings
          -> QiAWS (Response LBS.ByteString)
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
          -> QiAWS LBS.ByteString
        getS3ObjectContent S3Object{_s3oBucketId, _s3oKey = S3Key objKey} = do
          let bucketName = getPhysicalName config $ getById config _s3oBucketId
          r <- send . getObject (BucketName bucketName) $ ObjectKey objKey
          sinkBody (r ^. gorsBody) sinkLbs

        streamFromS3Object
          :: S3Object
          -> (Sink BS.ByteString LambdaProgram a)
          -> QiAWS a
        streamFromS3Object S3Object{_s3oBucketId, _s3oKey = S3Key objKey} sink = do
          let bucketName = getPhysicalName config $ getById config _s3oBucketId
          source <- transPipe liftAWSFromResourceIO . fst <$> (
                      liftAWSFromResourceIO . unwrapResumable . _streamBody . (^.gorsBody)
                  =<< (send $ getObject (BucketName bucketName) $ ObjectKey objKey)
                )
          source $$ transPipe interpret sink


        streamS3Objects
            :: S3Object
            -> S3Object
            -> (Conduit BS.ByteString LambdaProgram BS.ByteString)
            -> QiAWS ()
        streamS3Objects
          fromS3Obj@S3Object{_s3oKey  = S3Key fromObjKey}
          toS3Obj@S3Object{_s3oKey    = S3Key toObjKey}
          cond = do

            let fromBucketName = getPhysicalName config $ getById config $ fromS3Obj^.s3oBucketId
                toBucketName = getPhysicalName config $ getById config $ toS3Obj^.s3oBucketId
                sink = streamUpload $ createMultipartUpload (BucketName toBucketName) (ObjectKey toObjKey)
                conduit = transPipe interpret cond
                -- not sure if this will help with memory leaks
                {- gcConduit = awaitForever $ \i -> do -}
                              {- liftIO $ performMajorGC -}
                              {- yield i -}

            source <- transPipe liftAWSFromResourceIO . fst <$> (
                      liftAWSFromResourceIO . unwrapResumable . _streamBody . (^.gorsBody)
                  =<< (send $ getObject (BucketName fromBucketName) $ ObjectKey fromObjKey)
                )

            {- void $ source =$= gcConduit =$= conduit $$ sink -}
            void $ source =$= conduit $$ sink

        putS3ObjectContent
          :: S3Object
          -> LBS.ByteString
          -> QiAWS ()
        putS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key objKey} content = do
          r <- send . putObject (BucketName bucketName) (ObjectKey objKey) $ toBody content
          return ()

          where
            bucketName = getPhysicalName config $ getById config _s3oBucketId


-- DynamoDB
        scanDdbRecords
          :: DdbTableId
          -> QiAWS ScanResponse
        scanDdbRecords ddbTableId = do
          send $ scan tableName

          where
            tableName = getPhysicalName config $ getById config ddbTableId


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
            tableName = getPhysicalName config $ getById config ddbTableId


        getDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS GetItemResponse
        getDdbRecord ddbTableId keys = do
          send $ getItem tableName & giKey .~ keys

          where
            tableName = getPhysicalName config $ getById config ddbTableId


        putDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS PutItemResponse
        putDdbRecord ddbTableId item = do
          send $ putItem tableName & piItem .~ item

          where
            tableName = getPhysicalName config $ getById config ddbTableId


        deleteDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS DeleteItemResponse
        deleteDdbRecord ddbTableId key = do
          send $ deleteItem tableName & diKey .~ key

          where
            tableName = getPhysicalName config $ getById config ddbTableId

-- Util
        say
          :: Text
          -> QiAWS ()
        say = logMessage

