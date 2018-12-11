{-# LANGUAGE DataKinds #-}

module Qi.Test.Config.Eff where

import           Control.Lens
import           Control.Monad.Freer           hiding (run)
import           Control.Monad.Freer.State
import           Data.Default                  (def)
import qualified Data.HashMap.Strict           as SHM
import           Protolude                     hiding (State, get, put,
                                                runState)
import           Qi.AWS.Types                  (AwsMode (LocalStack))
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang
import qualified Qi.Program.Wiring.IO          as IO
import           Qi.Test.Logger
import           Test.Tasty.Hspec


spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    it "works" $ do
      let exec = IO.run def LocalStack mkTestLogger $ do
            s3Bucket "mybucket" def
            getConfig

          expected = def &
                        (s3Config . s3IdToBucket  .~ SHM.singleton (S3BucketId 0) (S3Bucket "mybucket" def []))
                      . (s3Config . s3NameToId    .~ SHM.singleton "mybucket" (S3BucketId 0) )
                      . (nextId .~ 1)

      exec `shouldReturn` expected


