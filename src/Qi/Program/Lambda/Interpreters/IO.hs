{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Qi.Program.Lambda.Interpreters.IO (run) where

--import           Network.AWS.S3.StreamingUpload
import           Control.Concurrent                    hiding (yield)
import           Control.Concurrent.STM
import           Control.Exception.Lens                (handling)
import           Control.Lens                          hiding (view)
import           Control.Monad.Base                    (MonadBase)
import           Control.Monad.Catch                   (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Operational             (ProgramViewT ((:>>=), Return),
                                                        singleton, view)
import           Control.Monad.Reader.Class            (MonadReader)
import           Control.Monad.Trans.AWS               (AWST, runAWST, send)
import           Control.Monad.Trans.Resource          (MonadResource,
                                                        ResourceT)
import           Data.Aeson                            (FromJSON, ToJSON,
                                                        Value (..), decode,
                                                        encode, object)
import           Data.Binary.Builder                   (fromLazyByteString,
                                                        toLazyByteString)
import qualified Data.ByteString.Char8                 as BS
import qualified Data.ByteString.Lazy.Char8            as LBS
import           Data.Conduit                          (Conduit, Sink,
                                                        awaitForever, transPipe,
                                                        yield, ($$), (=$=))
import           Data.Conduit.Binary                   (sinkLbs)
import qualified Data.Conduit.List                     as CL
import           Data.Default                          (def)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (decodeUtf8)
import qualified Data.Text.IO                          as T
import           Data.Time.Clock                       (UTCTime)
import qualified Data.Time.Clock                       as C
import           Data.Vector                           (Vector)
import qualified Data.Vector                           as V
import           GHC.Exts                              (fromList)
import           Network.AWS                           hiding (Request,
                                                        Response, send)
import           Network.AWS.Data.Body                 (RsBody (..), fuseStream)
import           Network.AWS.Data.Text                 (ToText (..))
import           Network.AWS.DynamoDB
import           Network.AWS.Lambda
import           Network.AWS.S3
import           Network.AWS.S3.CreateMultipartUpload
import           Network.AWS.SQS
import           Network.HTTP.Client                   (ManagerSettings,
                                                        Request, Response,
                                                        httpLbs, newManager)
import           Protolude                             hiding ((<&>))
import           Qi.Amazonka                           (currentRegion)
import           Qi.AWS.SQS
import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface           (LambdaInstruction (..),
                                                        LambdaProgram)
import           Qi.Program.Lambda.Interpreters.IO.Log
import           Qi.Util                               (time)
import           Servant.Client                        (BaseUrl, ClientM,
                                                        ServantError,
                                                        mkClientEnv, runClientM)
import           System.IO                             (stdout)
import           System.Mem                            (performMajorGC)


newtype QiAWS a = QiAWS {unQiAWS :: AWST (ResourceT IO) a}
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadResource
    , MonadReader Env
    , MonadAWS
    )


data LoggerType = NoLogger | StdOutLogger | CwLogger

loggerType :: LoggerType
loggerType = CwLogger
{- loggerType = NoLogger -}

withEnv
  :: Text
  -> Config
  -> (Env -> (Text -> QiAWS ()) -> IO ())
  -> IO ()
withEnv name config action =
  case loggerType of
    CwLogger -> do
      logQueue <- liftIO . atomically $ newTBQueue 1000
      loggerDone <- liftIO . forkIOSync $ cloudWatchLoggerWorker name config logQueue

      let cloudWatchLogger :: Logger
          cloudWatchLogger level bd = do
            let prefix = LBS.fromChunks ["[lambda][amazonka][]", "[", BS.pack $ show level, "] "]
                msg = fromLazyByteString prefix <> bd
            queueLogEntry logQueue . decodeUtf8 . LBS.toStrict $ toLazyByteString msg

      env <- newEnv Discover <&> set envRegion currentRegion . set envLogger cloudWatchLogger

      let logMessage = liftIO . queueLogEntry logQueue . T.append "[Message] "
      action env logMessage

      signalLogEnd logQueue
      when (config^.waitOnLogger) $
        takeMVar loggerDone -- wait on the logger to finish logging messages

    StdOutLogger -> do
      logger <- newLogger Debug stdout
      env <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion

      let logMessage = liftIO . putStrLn . T.unpack . T.append "[Message] "
      action env logMessage

    NoLogger -> do
      env <- newEnv Discover <&> set envRegion currentRegion
      action env . const $ return ()


run
  :: Text
  -> Config
  -> LambdaProgram LBS.ByteString
  -> IO ()
run name config program = do
  withEnv name config $ \env logMessage ->
    runResourceT . runAWST env . unQiAWS $
      void . liftIO . LBS.putStr =<< go logMessage program

  where
    go
      :: (Text -> QiAWS ())
      -> LambdaProgram a
      -> QiAWS a
    go logMessage = interpret
      where
        interpret
          :: LambdaProgram a
          -> QiAWS a
        interpret program' =
          case view program' of

            GetAppName :>>= is ->
              getAppName >>= interpret . is

-- Http
            Http ms req :>>= is ->
              http ms req >>= interpret . is

-- Servant
            RunServant ms baseUrl req :>>= is ->
              runServant ms baseUrl req >>= interpret . is

-- Amazonka
            AmazonkaSend cmd :>>= is ->
              amazonkaSend cmd >>= interpret . is


-- Lambda
            InvokeLambda lbd payload :>>= is ->
              invokeLambda lbd payload >>= interpret . is

-- S3
            GetS3ObjectContent s3Obj :>>= is ->
              getS3ObjectContent s3Obj >>= interpret . is
{-
            StreamFromS3Object s3Obj sink :>>= is ->
              streamFromS3Object s3Obj sink >>= interpret . is

            StreamS3Objects inS3Obj outS3Obj conduit :>>= is ->
              streamS3Objects inS3Obj outS3Obj conduit >>= interpret . is
-}
            PutS3ObjectContent s3Obj content :>>= is ->
              putS3ObjectContent s3Obj content >>= interpret . is

            ListS3Objects s3Bucket cont :>>= is ->
              listS3Objects s3Bucket (\acc -> interpret . cont acc) >>= interpret . is

-- DDB
            ScanDdbRecords ddbTableId :>>= is ->
              scanDdbRecords ddbTableId >>= interpret . is

            QueryDdbRecords ddbTableId keyCond expAttrs :>>= is ->
              queryDdbRecords ddbTableId keyCond expAttrs >>= interpret . is

            GetDdbRecord ddbTableId keys :>>= is ->
              getDdbRecord ddbTableId keys >>= interpret . is

            PutDdbRecord ddbTableId item :>>= is ->
              putDdbRecord ddbTableId item >>= interpret . is

            DeleteDdbRecord ddbTableId key :>>= is ->
              deleteDdbRecord ddbTableId key >>= interpret . is

-- SQS
            SendMessage qid a :>>= is ->
              sendSqsMessage qid a >>= interpret . is

            ReceiveMessage qid :>>= is ->
              receiveSqsMessage qid >>= interpret . is

            DeleteMessage qid rh :>>= is ->
              deleteSqsMessage qid rh >>= interpret . is


-- Util
            Say text :>>= is ->
              say text >>= interpret . is

            Sleep micros :>>= is ->
              sleep micros >>= interpret . is

            GetCurrentTime :>>= is ->
              getCurrentTime >>= interpret . is

            Return x ->
              return x




        getAppName
          :: QiAWS Text
        getAppName =
          return $ config^.namePrefix

-- Http

        http
          :: ManagerSettings
          -> Request
          -> QiAWS (Response LBS.ByteString)
        http ms req = liftIO $
          httpLbs req =<< newManager ms

-- Servant
        runServant
          :: ManagerSettings
          -> BaseUrl
          -> ClientM a
          -> QiAWS (Either ServantError a)
        runServant ms baseUrl req = liftIO $ do
          mgr <- newManager ms
          runClientM req $ mkClientEnv mgr baseUrl

-- Amazonka
        amazonkaSend
          :: (AWSRequest a)
          => a
          -> QiAWS (Rs a)
        amazonkaSend = send

-- Lambda
        invokeLambda
          :: ToJSON a
          => LambdaId
          -> a
          -> QiAWS ()
        invokeLambda id payload = do
          send $ invoke pname (toS $ encode payload) & iInvocationType ?~ Event
          pure ()
          where
            pname = getPhysicalName config $ getById config id


-- S3
        getS3ObjectContent
          :: S3Object
          -> QiAWS (Either Text LBS.ByteString)
        getS3ObjectContent S3Object{_s3oBucketId, _s3oKey = S3Key objKey} =
          handling _KeyNotFound handler action

          where
            bucketName = getPhysicalName config $ getById config _s3oBucketId

            action = do
              r <- send . getObject (BucketName bucketName) $ ObjectKey objKey
              fmap Right $ (r ^. gorsBody) `sinkBody` sinkLbs


            handler _ = pure $ Left "KeyNotFound"

            _KeyNotFound :: AsError a => Getting (First ServiceError) a ServiceError
            _KeyNotFound = _ServiceError . hasStatus 404 -- . hasCode "InvalidKeyPair.Duplicate"



{-
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
            panic "s3 streams are not implemented"
            {- let fromBucketName = getPhysicalName config $ getById config $ fromS3Obj^.s3oBucketId -}
                {- toBucketName = getPhysicalName config $ getById config $ toS3Obj^.s3oBucketId -}
                {- sink = streamUpload $ createMultipartUpload (BucketName toBucketName) (ObjectKey toObjKey) -}
                {- conduit = transPipe interpret cond -}

            {- source <- transPipe liftAWSFromResourceIO . fst <$> ( -}
                      {- liftAWSFromResourceIO . unwrapResumable . _streamBody . (^.gorsBody) -}
                  {- =<< (send $ getObject (BucketName fromBucketName) $ ObjectKey fromObjKey) -}
                {- ) -}

            {- void $ source =$= conduit $$ sink -}
-}

        putS3ObjectContent
          :: S3Object
          -> LBS.ByteString
          -> QiAWS ()
        putS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key objKey} content = do
          r <- send $ putObject (BucketName bucketName) (ObjectKey objKey) (toBody content)
                        & poACL ?~ OPublicReadWrite
          pure ()

          where
            bucketName = getPhysicalName config $ getById config _s3oBucketId

        listS3Objects
          :: Monoid a
          => S3BucketId
          -> (a -> Vector S3Object -> QiAWS a)
          -> QiAWS a
        listS3Objects bucketId cont =
          go mempty Nothing

          where
            go acc maybeToken = do
              r <- send $ case maybeToken of
                            Nothing -> -- first pagination call
                              listObjectsV2 (BucketName bucketName)
                            Just token ->
                              listObjectsV2 (BucketName bucketName) & lovContinuationToken ?~ token


              acc' <- cont acc $ (\o -> S3Object bucketId $ S3Key . toText $ o ^. oKey) <$> V.fromList (r ^. lovrsContents)
              case r ^. lovrsNextContinuationToken of
                Just token ->
                  go acc' (Just token)
                Nothing ->
                  pure acc'


            bucketName = getPhysicalName config $ getById config bucketId

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

-- SQS
        sendSqsMessage
          :: ToJSON a
          => SqsQueueId
          -> a
          -> QiAWS ()
        sendSqsMessage queueId msg = do
          send . sendMessage queueUrl . toS $ encode msg
          pure ()

          where
            queueUrl = getPhysicalName config $ getById config queueId


        receiveSqsMessage
          :: FromJSON a
          => SqsQueueId
          -> QiAWS [(a, ReceiptHandle)] -- the json body and the receipt handle
        receiveSqsMessage queueId = do
          r <- send $ receiveMessage queueUrl
          pure . catMaybes $ (\m -> do
                                msg <- decode . toS =<< m ^. mBody
                                rh <- m ^. mReceiptHandle
                                pure (msg, ReceiptHandle rh)
                              ) <$> (r ^. rmrsMessages)

          where
            queueUrl = getPhysicalName config $ getById config queueId


        deleteSqsMessage
          :: SqsQueueId
          -> ReceiptHandle
          -> QiAWS ()
        deleteSqsMessage = undefined

-- Util
        say
          :: Text
          -> QiAWS ()
        say = logMessage

        sleep
          :: Int
          -> QiAWS ()
        sleep = liftIO . threadDelay

        getCurrentTime
          :: QiAWS UTCTime
        getCurrentTime = liftIO C.getCurrentTime
