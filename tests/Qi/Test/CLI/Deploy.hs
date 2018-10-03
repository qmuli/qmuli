{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Test.CLI.Deploy where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Default                  (Default, def)
import qualified Data.HashMap.Strict           as SHM
import           Protolude                     hiding (Reader, State, get, put,
                                                runReader, runState)
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import qualified Qi.Program.CF.Ipret.Gen       as CF
import           Qi.Program.CF.Lang
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang
import           Qi.Program.Gen.Lang
import qualified Qi.Program.Lambda.Ipret.Gen   as Lbd
import qualified Qi.Program.S3.Ipret.Gen       as S3
import           Qi.Program.S3.Lang
import qualified Qi.Program.Wiring.IO          as IO
import           Qi.Test.Ipret
import           Test.Tasty.Hspec


execName :: Text
execName = "qmuli"


spec :: Spec
spec = parallel $ do
  describe "Deploy" $ do
    let params = Params {
            config = def :: Config
          , stacks = mempty
          }


    it "works" $ do
      let renderedTemplate = "rendered template"
          lambdaBinary = "lambda binary"
          action = deployApp renderedTemplate lambdaBinary
          expectedJournal = def{
              s3Actions = [ CreateBucketAction "app"
                          , PutContentAction (S3Object {_s3oBucketId = S3BucketId 1, _s3oKey = S3Key "cf.json"}) renderedTemplate
                          , PutContentAction (S3Object {_s3oBucketId = S3BucketId 1, _s3oKey = S3Key "lambda.zip"}) lambdaBinary
                          ]
            , logs = ["deploying the app..."]
            }
          (config', journal) = testRun params action

      journal `shouldBe` expectedJournal
      config' `shouldBe` def


  describe "Create stack" $ do
    let params = def{
            config
          }
        config = snd . run . runState def . Config.run $ do
          s3Bucket "app"

    it "works" $ do
      let action = createCfStack
          expectedJournal = def{
              cfActions = [ CreateStackAction (StackName execName) (S3Object {_s3oBucketId = S3BucketId 0, _s3oKey = S3Key "cf.json"})]
            , logs =  [ "creating the stack..."
                      , "waiting on the stack to be created..."
                      , "stack was successfully created"
                      ]
            }
          (config', journal) = testRun params action

      journal `shouldBe` expectedJournal
      config' `shouldBe` config



