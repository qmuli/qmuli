{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.S3.Event where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import qualified Data.Text                  as T
import           Protolude

import           Control.Monad.Fail         (fail)
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors

parse
  :: Value
  -> Config
  -> Parser S3Event
parse (Object e) config = do
  firstRecord <- headMay <$> e .: "Records"
  case firstRecord of
    Nothing -> fail "no records"
    Just record -> do
      s3          <- record .: "s3"
      bucketName  <- (.: "name") =<< s3 .: "bucket"
      key         <- (.: "key")  =<< s3 .: "object"

      pure . S3Event $ S3Object (getIdByName config $ removeDotNamePrefix bucketName) (S3Key key)

removeDotNamePrefix
  :: Text
  -> Text
removeDotNamePrefix name = T.drop prefixLength name
  where
    prefixLength = 1 + T.length (T.takeWhile (/='.') name)


