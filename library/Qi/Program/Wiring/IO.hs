{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Qi.Program.Wiring.IO  where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Protolude                     hiding (State, runState, (<&>))
import           Qi.Config.AWS
import qualified Qi.Program.CF.Ipret.Gen       as CF
import           Qi.Program.CF.Lang            (CfEff)
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang        (ConfigEff)
import qualified Qi.Program.Gen.Ipret.IO       as Gen
import           Qi.Program.Gen.Lang           (GenEff)
import qualified Qi.Program.Lambda.Ipret.Gen   as Lbd
import           Qi.Program.Lambda.Lang        (LambdaEff)
import qualified Qi.Program.S3.Ipret.Gen       as S3
import           Qi.Program.S3.Lang            (S3Eff)







run
  :: Text
  -> Config
  -> Eff '[CfEff, S3Eff, LambdaEff, GenEff, ConfigEff, State Config, IO] a
  -> IO a
run name config =
    runM
  . map fst
  . runState config
  . Config.run
  . Gen.run Gen.LocalStack
  . Lbd.run
  . S3.run
  . CF.run

{-
--import           Network.AWS.S3.StreamingUpload
import           Codec.Archive.Zip
import           Control.Concurrent           hiding (yield)
import           Control.Concurrent.STM
import           Control.Exception.Lens       (handling)
import           Control.Lens                 hiding (view, (.=))
import           Control.Monad.Base           (MonadBase)
import           Control.Monad.Catch          (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Operational    (ProgramViewT ((:>>=), Return),
                                               singleton, view)
import           Control.Monad.Reader.Class   (MonadReader)
import           Control.Monad.Trans.AWS      (AWST, runAWST, send)
import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
import           Data.Aeson                   (FromJSON, ToJSON, Value (..),
                                               decode, encode, object, (.=))
import           Data.Binary.Builder          (fromLazyByteString,
                                               toLazyByteString)
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.Conduit                 (Conduit, Sink, awaitForever,
                                               transPipe, yield, ($$), (=$=))
import           Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit.List            as CL
import           Data.Default                 (def)
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8)
import qualified Data.Text.IO                 as T
import           Data.Time.Clock              (UTCTime)
import qualified Data.Time.Clock              as C
import           GHC.Exts                     (fromList)
import           Network.AWS                  hiding (Request, Response, send)
import           Network.AWS.Data.Body        (RsBody (..), fuseStream)
import           Network.AWS.Data.Text        (ToText (..))
import           Network.AWS.DynamoDB
import           Network.AWS.Lambda
import           Network.AWS.LexModels
import           Network.AWS.LexModels.Types
import           Network.AWS.S3
import           Network.AWS.S3.Types         (ETag)
import           Network.AWS.SQS
import           Network.HTTP.Client          (ManagerSettings, Request,
                                               Response, httpLbs, newManager)
import           Protolude                    hiding ((<&>))
import           Qi.Amazonka                  (currentRegion)
import           Qi.AWS.Lex
import           Qi.AWS.SQS
import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface  (LambdaInstruction (..),
                                               LambdaProgram)
import           Qi.Util                      (time)
import           Servant.Client               (BaseUrl, ClientM, ServantError,
                                               mkClientEnv, runClientM)
import           System.IO                    (hFlush, stdout)
import           System.Mem                   (performMajorGC)



-- S3


    multipartS3Upload
      :: S3Object
      -> (S3Object -> Text -> QiAWS [(Int, ETag)])
      -> QiAWS ()
    multipartS3Upload sinkS3Object@S3Object{ _s3oBucketId, _s3oKey = S3Key (ObjectKey -> sinkObjectKey) } cont = do
      r <- send $ createMultipartUpload sinkBucketName sinkObjectKey
      case r ^. cmursUploadId of
        Nothing ->
          panic "no uploadId returned"
        Just uploadId -> do

          chunks <- cont sinkS3Object uploadId
          case chunks of
            [] -> -- no chunks succeeded, abort the upload
              void . send $ abortMultipartUpload sinkBucketName sinkObjectKey uploadId
            (x:xs) -> do
              let completedParts = NE.map (\(i, etag) -> completedPart i etag) (x:|xs)
                  cmu = completedMultipartUpload & cmuParts ?~ completedParts

              send $ completeMultipartUpload sinkBucketName sinkObjectKey uploadId
                        & cMultipartUpload ?~ cmu

              pass

      where
        sinkBucketName = BucketName . getPhysicalName config $ getById config _s3oBucketId

    uploadS3Chunk
      :: S3Object -- sink
      -> Text -- uploadId
      -> (Int, S3Object) -- source chunk
      -> QiAWS (Maybe (Int, ETag))
    uploadS3Chunk
      sinkS3Object@S3Object{_s3oKey = S3Key (ObjectKey -> sinkObjectKey)}
      uploadId
      (i, sourceS3Object@S3Object{_s3oKey = S3Key sourceObjectTextKey}) = do
        r <- send $ uploadPartCopy sinkBucketName source sinkObjectKey i uploadId
        let cpr = r ^. upcrsCopyPartResult

        pure . map (i, ) $ cpr >>= (\c -> c ^. cprETag)

      where
        sinkBucketName = BucketName . getPhysicalName config $ getById config $ sinkS3Object ^. s3oBucketId

        sourceBucketTextName = getPhysicalName config $ getById config $ sourceS3Object ^. s3oBucketId
        source = sourceBucketTextName <> "/" <> sourceObjectTextKey

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

-- Lex

    startBotImport
      :: BotSpec
      -> QiAWS ()
    startBotImport botSpec = do
      archive <- liftIO . compress . toS $ encode botSpec
      void . send $ startImport
                      archive
                      Bot
                      OverwriteLatest

      where
        compress :: ByteString -> IO ByteString
        compress content = do
          let tempFile      = "temp.zip"
              fileInArchive = "spec.json"
          mkEntrySelector fileInArchive
            >>= createArchive tempFile . addEntry Store content
          BS.readFile tempFile


-}
