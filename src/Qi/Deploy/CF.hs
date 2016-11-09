{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.CF (create, destroy) where

import           Control.Lens
import           Control.Monad.Trans.AWS (runAWST, send)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text)
import qualified Data.Text               as T 
import           Network.AWS             hiding (send)
import           Network.AWS.CloudFormation
import           System.IO               (stdout)

import           Qi.Amazonka             (runAmazonka)


create
  :: Text
  -> IO ()
create appName = do
  runAmazonka $ do
    _ <- send $ createStack appName 
                  & csTemplateURL ?~ T.concat ["https://s3.amazonaws.com/", appName, "/cf.json"]
                  & csCapabilities .~ [CapabilityNamedIAM]
    return ()


destroy
  :: Text
  -> IO ()
destroy appName = do
  runAmazonka $ do
    _ <- send $ deleteStack appName
                  & dsRetainResources .~ []
    return ()

    
