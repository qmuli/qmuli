{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Deploy.CF (create, describe, destroy) where

import           Control.Lens
import           Control.Monad.Trans.AWS          (runAWST, send)
import           Data.Aeson                       (ToJSON)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           GHC.Generics
import           Network.AWS                      hiding (send)
import           Network.AWS.CloudFormation
import           Network.AWS.CloudFormation.Types
import           System.IO                        (stdout)

import           Qi.Amazonka                      (runAmazonka)


data StackDescription = StackDescription {
    sdStatus  :: Text
  , sdOutputs :: [(Text, Text)]
  } deriving (Generic, Show)
instance ToJSON StackDescription

create
  :: Text
  -> IO ()
create appName =
  runAmazonka $ do
    _ <- send $ createStack appName
                  & csTemplateURL ?~ T.concat ["https://s3.amazonaws.com/", appName, "/cf.json"]
                  & csCapabilities .~ [CapabilityNamedIAM]
    return ()

describe
  :: Text
  -> IO StackDescription
describe appName =
  runAmazonka $ do
    r <- send $ describeStacks
                  & dStackName ?~ appName
    -- assume one stack is returned
    let stack = head $ r^.dsrsStacks
        outputs = stack^.sOutputs
    return $ StackDescription {
        sdStatus = T.pack . show $ stack^.sStackStatus
      , sdOutputs = map (\o -> (fromJust $ o^.oOutputKey, fromJust $ o^.oOutputValue)) outputs
      }

destroy
  :: Text
  -> IO ()
destroy appName =
  runAmazonka $ do
    _ <- send $ deleteStack appName
                  & dsRetainResources .~ []
    return ()


