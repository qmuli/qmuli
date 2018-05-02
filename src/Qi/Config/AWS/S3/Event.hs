{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.S3.Event where

import           Control.Lens
import           Control.Monad.Fail         (fail)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import qualified Data.Text                  as T
import           Protolude
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors


parse
  :: Config
  -> Value
  -> Parser S3Event
parse config = withObject "S3Event" $ \o -> do
  firstRecord <- headMay <$> o .: "Records"
  case firstRecord of
    Nothing ->
      fail "no records"
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

