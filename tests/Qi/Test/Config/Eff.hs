{-# LANGUAGE DataKinds #-}

module Qi.Test.Config.Eff where

import           Control.Lens
import           Control.Monad.Freer           hiding (run)
import           Control.Monad.Freer.State
import           Data.Default                  (def)
import qualified Data.HashMap.Strict           as SHM
import           Protolude                     hiding (State, get, put,
                                                runState)
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang
import qualified Qi.Program.Wiring.IO          as IO
import           Test.Tasty.Hspec


spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    it "works" $ do
      let exec = IO.run "test" def $ do
            s3Bucket "mybucket"
            getConfig

          expected = def &
                        (s3Config . s3Buckets . s3IdToBucket .~ SHM.singleton (S3BucketId 0) (S3Bucket "mybucket" []))
                      . (s3Config . s3Buckets . s3NameToBucketId .~ SHM.singleton "mybucket" (S3BucketId 0) )
                      . (nextId .~ 1)

      exec `shouldReturn` expected


