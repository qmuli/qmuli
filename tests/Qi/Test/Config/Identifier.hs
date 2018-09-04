{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Test.Config.Identifier where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Default                  (def)
import qualified Data.HashMap.Strict           as SHM
import           Protolude                     hiding (runState)
import           Qi.Config.AWS                 (Config (..), s3Config)
import           Qi.Config.AWS.S3              (s3Buckets, s3idxIdToS3Bucket)
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang        (ConfigEff, s3Bucket)
import           Test.Tasty.Hspec


appName :: Text
appName = "testName"

configProgram
  :: Member ConfigEff effs
  => Eff effs ()
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
      . run
      . runState def{ _namePrefix = appName }
      . Config.run $ configProgram


