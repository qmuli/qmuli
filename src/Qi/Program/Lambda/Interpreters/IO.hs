{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interpreters.IO (run) where

import           Control.Lens                 hiding (view)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Operational
import           Control.Monad.Trans.AWS      (AWST, runAWST, send)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson                   (Value (..), encode, object)
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Conduit                 as C
import           Data.Conduit.Binary          (sinkLbs)
import           Data.Default                 (def)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Network.AWS                  hiding (send)
import           Network.AWS.DynamoDB         as A
import qualified Network.AWS.S3               as A
import           System.IO                    (stdout)

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

type QiAWS = AWST (ReaderT Config (ResourceT IO))

run
  :: LambdaProgram ()
  -> Config
  -> IO ()
run program config = do
  env <- newEnv Discover <&> set envRegion currentRegion
  runResourceT . (`runReaderT` config) . runAWST env $ interpret program

  where
    interpret
      :: LambdaProgram ()
      -> QiAWS ()
    interpret program = do
      case view program of

-- S3
        (GetS3ObjectContent s3Obj) :>>= is -> do
          interpret . is =<< getS3ObjectContent s3Obj

        (PutS3ObjectContent s3Obj content) :>>= is -> do
          interpret . is =<< putS3ObjectContent s3Obj content

-- DDB
        (ScanDdbRecords ddbTableId) :>>= is -> do
          interpret . is =<< scanDdbRecords ddbTableId

        (QueryDdbRecords ddbTableId keyCond expAttrs) :>>= is -> do
          interpret . is =<< queryDdbRecords ddbTableId keyCond expAttrs

        (GetDdbRecord ddbTableId keys) :>>= is -> do
          interpret . is =<< getDdbRecord ddbTableId keys

        (PutDdbRecord ddbTableId item) :>>= is -> do
          interpret . is =<< putDdbRecord ddbTableId item

        (DeleteDdbRecord ddbTableId key) :>>= is -> do
          interpret . is =<< deleteDdbRecord ddbTableId key

-- Util
        (Respond status content) :>>= is -> do
          respond status content -- final output, no more program instructions

        Return _ ->
          return def

      where
        {- say :: Show a => Text -> a -> QiAWS () -}
        {- say msg = liftIO . T.putStrLn . mappend msg . T.pack . show -}


-- S3
        getS3ObjectContent
          :: S3Object
          -> QiAWS LBS.ByteString
        getS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key objKey} = do
          {- putStrLn $ "GetS3ObjectContent: " ++ show s3Obj -}
          {- config <- lift ask -}
          {- say "Reading s3 object content..." "" -}
          r <- send . A.getObject (A.BucketName bucketName) $ A.ObjectKey objKey
          sinkBody (r ^. A.gorsBody) sinkLbs

          where
            bucketName = getFullBucketName bucket config
            bucket = getS3BucketById _s3oBucketId config

-- TODO: add a streaming version that takes a sink and streams the rsBody into it
-- sinkBody :: MonadResource m => RsBody -> Sink ByteString m a -> m a


        putS3ObjectContent
          :: S3Object
          -> LBS.ByteString
          -> QiAWS ()
        putS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key objKey} content = do
          {- putStrLn $ "PutS3ObjectContent: " ++ show s3Obj -}
          {- config <- lift ask -}
          {- say "Writing s3 object content..." "" -}
          r <- send . A.putObject (A.BucketName bucketName) (A.ObjectKey objKey) $ toBody content
          return ()

          where
            bucketName = getFullBucketName bucket config
            bucket = getS3BucketById _s3oBucketId config


-- DynamoDB
        scanDdbRecords
          :: DdbTableId
          -> QiAWS ScanResponse
        scanDdbRecords ddbTableId = do
          send $ A.scan tableName

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        queryDdbRecords
          :: DdbTableId
          -> Maybe Text
          -> DdbAttrs
          -> QiAWS QueryResponse
        queryDdbRecords ddbTableId keyCond expAttrs = do
          send $ A.query tableName
            & qKeyConditionExpression .~ keyCond
            & qExpressionAttributeValues .~ expAttrs

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        getDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS GetItemResponse
        getDdbRecord ddbTableId keys = do
          send $ A.getItem tableName & giKey .~ keys

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        putDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS PutItemResponse
        putDdbRecord ddbTableId item = do
          send $ A.putItem tableName & piItem .~ item

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


        deleteDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS DeleteItemResponse
        deleteDdbRecord ddbTableId key = do
          send $ A.deleteItem tableName & diKey .~ key

          where
            tableName = getFullDdbTableName (getDdbTableById ddbTableId config) config


-- Util
        respond
          :: Int
          -> Value
          -> QiAWS ()
        respond status content =
          liftIO . LBS.putStr . encode $ object [
              ("status", Number $ fromIntegral status)
            , ("body", content)
            ]




