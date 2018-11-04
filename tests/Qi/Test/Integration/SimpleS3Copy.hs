{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Qi.Test.Integration.SimpleS3Copy where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import qualified Data.ByteString.Lazy          as LBS
import           Data.Default                  (def)
import           Protolude                     hiding (State, getAll, runState)
import           Qi.AWS.Types                  (AwsMode (LocalStack))
import           Qi.CLI.Dispatcher
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda          (LambdaMemorySize (..),
                                                lpMemorySize)
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors    (getIdByName)
import qualified Qi.Config.CfTemplate          as CF
import           Qi.Config.Identifier          (S3BucketId)
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang        (ConfigEff, s3Bucket,
                                                s3BucketLambda)
import           Qi.Program.Gen.Lang           (GenEff, say, sleep)
import qualified Qi.Program.Gen.Lang           as Gen
import           Qi.Program.S3.Lang            (S3Eff, S3LambdaProgram,
                                                getContent, putContent)
import qualified Qi.Program.Wiring.IO          as IO
import           Qi.Test.Logger
import           System.RandomString
import           Test.Hspec


configProgram
  :: Member ConfigEff effs
  => Eff effs ()
configProgram = do

  -- create an "input" s3 bucket
  incoming <- s3Bucket "incoming" def

  -- create an "output" s3 bucket
  outgoing <- s3Bucket "outgoing" def

  -- create a lambda, which will copy an s3 object from "incoming" to "outgoing" buckets
  -- upon an S3 "Put" event.
  -- Attach the lambda to the "incoming" bucket such way so each time a file is uploaded to
  -- the bucket, the lambda is called with the information about the newly uploaded file.
  -- The lambda creation function takes the Lambda name, s3BucketId to attach to, lambda
  -- function itself and a lambda profile, that specifies attributes like memory size and
  -- timeout, and has meaningful defaults for those.
  void $ s3BucketLambda "copyS3Object" incoming (copyContentsLambda outgoing) $
    def & lpMemorySize .~ M1536

  where
    copyContentsLambda
      :: (Member S3Eff effs, Member GenEff effs)
      => S3BucketId
      -> S3LambdaProgram effs
    copyContentsLambda sinkBucketId = lbd
      where
        lbd event = do
          let incomingS3Obj = event ^. s3eObject
              outgoingS3Obj = s3oBucketId .~ sinkBucketId $ incomingS3Obj

          say "getting content"
          -- get the content of the newly uploaded file
          eitherContent <- getContent incomingS3Obj

          case eitherContent of
            Right content -> do
              say "putting content"
              -- write the content into a new file in the "output" bucket
              putContent outgoingS3Obj content

              pure "lambda had executed successfully"

            Left err ->
              pure . toS $ "error: '" <> err <> "'"


spec :: Spec
spec = parallel $ do
  describe "Integration" $ do

    let config =
            snd
          . run
          . runState def{_namePrefix = "qmuli-test"}
          . Config.run
          $ do
            {- s3Bucket "app" -- always assume existence of the app bucket -}
            configProgram

        template = CF.render config

    it "copies the file correctly" $ do
      filename <- randomString (StringOpts Base64 8)
      content <- randomString (StringOpts Base64 1024)

      result <- do
        -- deploy the stack
            IO.run "test" config LocalStack mkTestLogger $ do
              let incoming = s3Object (getIdByName config "incoming") $ S3Key filename
                  outgoing = s3Object (getIdByName config "outgoing") $ S3Key filename

              {- createCfStack template -}

              -- upload a file into the incoming bucket
              putContent incoming content

              sleep 1000000

              -- download the copied file from the outgoing bucket
              getContent outgoing


      -- verify that the file has been copied correctly
      result `shouldBe` Right content

{-
    it "works" $ do
      let content = "my content" :: Text

      result <- do
        -- deploy the stack
            IO.run "test" config LocalStack $ do
              builtLambda <- Gen.readFileLazy =<< Gen.build
              deployApp template builtLambda
              createCfStack template
              pure content

        -- upload a file into the incoming bucket
        -- download the copied file from the outgoing bucket
        -- verify that the file has been copied correctly

      result `shouldBe` content
-}
