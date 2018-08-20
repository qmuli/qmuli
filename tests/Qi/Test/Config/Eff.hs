module Qi.Test.Config.Eff where

import           Control.Lens
import           Control.Monad.Freer
import           Data.Default           (def)
import qualified Data.HashMap.Strict    as SHM
import           Protolude              hiding (State, runState)
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Config.Lang
import qualified Qi.Program.Wiring.IO   as IO
import           Test.Tasty.Hspec

spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    it "works" $ do
      let exec = IO.run "test" def $ do
            s3Bucket "mybucket"
            getConfig
      exec `shouldReturn`
        (s3Config . s3Buckets . s3idxIdToS3Bucket .~ SHM.singleton (S3BucketId 0) (S3Bucket "mybucket" [])) def
