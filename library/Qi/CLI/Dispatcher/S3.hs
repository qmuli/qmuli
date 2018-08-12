{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.CLI.Dispatcher.S3 where

import           Control.Lens
import           Control.Monad                (forM_, void, (<=<))
import           Control.Monad.IO.Class       (liftIO)
import           Data.List                    (intersect)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Network.AWS                  (AWS, send)
import           Network.AWS.Data.Body        (ToBody, toBody)
import           Network.AWS.S3               (BucketName (BucketName),
                                               ObjectKey (ObjectKey), bName,
                                               dObjects, dQuiet, delete',
                                               deleteObjects, lbrsBuckets,
                                               listObjectsV2, lovrsContents,
                                               oKey, objectIdentifier)
import qualified Network.AWS.S3               as S3
import           Network.AWS.S3.DeleteObjects
import           Network.AWS.S3.ListBuckets
import           Network.AWS.S3.ListObjectsV2
import           Protolude                    hiding (getAll)
import           Qi.Config.AWS                (Config, getAll, getAllWithIds,
                                               getById, getPhysicalName,
                                               namePrefix)
import           Qi.Config.AWS.S3             (S3Bucket, s3bName)
import           Qi.Config.Identifier         (S3BucketId)
import qualified Qi.Program.Gen.Lang          as I
import qualified Qi.Program.S3.Lang           as I


createBucket
  :: Text
  -> AWS ()
createBucket name =
  void $ send $ S3.createBucket (BucketName name)


putObject
  :: ToBody a
  => Text
  -> Text
  -> a
  -> AWS ()
putObject bucketName objectKey =
  void . send . S3.putObject (BucketName bucketName) (ObjectKey objectKey) . toBody

{-
clearBuckets
  :: Config
  -> I.LambdaProgram ()
clearBuckets config = do
  I.say "destroying buckets..."
  for_ bucketIds $ \bucketId -> do
    I.say $ "destroying bucket: '" <> (getById config bucketId) ^. s3bName <> "'"
    I.listS3Objects bucketId $ \_ -> I.deleteS3Objects

  where
    bucketIds :: [S3BucketId]
    bucketIds = map fst $ getAllWithIds config

-}
