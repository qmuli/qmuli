{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.S3.Event where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors


parse
  :: Value
  -> Config
  -> Parser S3Event
parse (Object e) config = do
    s3 <- (.: "s3") =<< head <$> e .: "Records"
    bucketName  <- (.: "name") =<< s3 .: "bucket"
    key         <- (.: "key")  =<< s3 .: "object"

    return . S3Event $ S3Object (getS3BucketIdByName (removeNamePrefix bucketName) config) (S3Key key)

removeNamePrefix
  :: String
  -> Text
removeNamePrefix name = T.pack $ drop prefixLength name
  where
    prefixLength = 1 + length (takeWhile (/='-') name)


