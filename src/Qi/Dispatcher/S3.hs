{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Dispatcher.S3 where

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
                                               listObjectsV, lrsContents, oKey,
                                               objectIdentifier)
import qualified Network.AWS.S3               as S3
import           Network.AWS.S3.DeleteObjects
import           Network.AWS.S3.ListBuckets
import           Network.AWS.S3.ListObjectsV

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


clearBuckets
  :: [Text]
  -> AWS ()
clearBuckets names = do
  existingBucketNames <- map (^.bName) . (^.lbrsBuckets) <$> send listBuckets
  let filteredBucketNames = map BucketName names `intersect` existingBucketNames

  forM_ filteredBucketNames $ \bucketName@(BucketName name) -> do
    liftIO $ putStrLn $ "cleaning up bucket: " ++ T.unpack name ++ "..."
    objectKeys <- map (^.oKey) . (^.lrsContents) <$> send (listObjectsV bucketName)
    if null objectKeys
      then
        return ()
      else do
        let objectNamesToDestroy = map (\(ObjectKey key) -> key) objectKeys
        liftIO $ putStrLn $ "\tfound s3 objects to destroy: " ++ show objectNamesToDestroy
        void . send . deleteObjects bucketName $ delete'
                                            & dQuiet ?~ True
                                            & dObjects .~ map objectIdentifier objectKeys


