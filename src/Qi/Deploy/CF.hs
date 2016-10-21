{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.CF (deploy) where

import           Control.Lens               hiding (view)
import           Control.Monad.Trans.AWS    (AWST, runAWST, send)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import           Network.AWS                hiding (send)
import qualified Network.AWS.CloudFormation as A
import qualified Network.AWS.S3             as A
import           Prelude                    hiding (log)
import           System.IO                  (stdout)
import           Turtle                     hiding (stdout)


log = liftIO . echo

toTextIgnore x = case toText x of
  Right s -> s
  Left err -> ""

deploy
  :: Text
  -> LBS.ByteString
  -> IO ()
deploy appName json = sh $ do
  createDirs
  liftIO $ uploadToS3 json

  where
    createDirs = do
        mapM_ ensureDir [".deploy"]
      where
        ensureDir dirName = do
          res <- testdir dirName
          if res
            then
              log $ T.concat ["'", toTextIgnore dirName, "' dir already exists, skipping creation"]
            else do
              log "creating dir..."
              mkdir dirName

    uploadToS3
      :: LBS.ByteString
      -> IO ()
    uploadToS3 json = do
      env <- newEnv NorthVirginia Discover
      logger <- newLogger Debug stdout
      runResourceT . runAWST (env & envLogger .~ logger) $ do
        r <- send $ A.putObject (A.BucketName appName) (A.ObjectKey "cf.json") $ toBody json
        return ()

    {- createStack :: IO () -}
    {- createStack = do -}
      {- env <- newEnv NorthVirginia Discover -}
      {- logger <- newLogger Debug stdout -}
      {- runResourceT . runAWST (env & envLogger .~ logger) $ do -}
        {- r <- send $ A.createStack (A.BucketName appName) (A.ObjectKey "cf.json") $ toBody json -}
        {- return () -}


