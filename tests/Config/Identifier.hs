{-# LANGUAGE OverloadedStrings #-}

module Config.Identifier where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import           Data.Default                         (def)
import qualified Data.HashMap.Strict                  as SHM
import           Protolude                            hiding (runState)
import           Qi.Config.AWS                        (Config, namePrefix,
                                                       s3Config)
import           Qi.Config.AWS.S3                     (s3Buckets,
                                                       s3idxIdToS3Bucket)
import           Qi.Program.Config.Interface          (ConfigProgram, s3Bucket)
import qualified Qi.Program.Config.Interpreters.Build as CB
import           Test.Tasty.Hspec


appName :: Text
appName = "testName"

configProgram :: ConfigProgram ()
configProgram = do
  void $ s3Bucket "bucket1"
  void $ s3Bucket "bucket2"
  void $ s3Bucket "bucket3"


spec :: Spec
spec = parallel $
  describe "Internal Config identifier" $ do

    it "is unique" $ do
      idCount `shouldBe` 3

  where

    idCount = length . SHM.elems $ config ^. s3Config . s3Buckets . s3idxIdToS3Bucket

    config =
        snd
      . (`runState` (def & namePrefix .~ appName))
      . CB.unQiConfig
      $ CB.interpret configProgram

