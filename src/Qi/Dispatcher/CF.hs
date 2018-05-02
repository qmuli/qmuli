{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Dispatcher.CF where

import           Control.Concurrent         (threadDelay)
import           Control.Lens
import           Control.Monad              (void, (<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (ToJSON)
import           Data.Maybe                 (fromJust, listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import           Network.AWS                (AWS, send)
import           Network.AWS.CloudFormation (Capability (CapabilityNamedIAM), StackStatus (SSCreateComplete, SSDeleteComplete, SSUpdateComplete),
                                             StackStatus, csCapabilities,
                                             csTemplateURL, dStackName,
                                             dsRetainResources, dsrsStacks,
                                             lsrsStackSummaries, oOutputKey,
                                             oOutputValue, sOutputs,
                                             sStackStatus, ssStackName,
                                             ssStackStatus, usCapabilities,
                                             usTemplateURL)
import qualified Network.AWS.CloudFormation as CF
import           Protolude
import           Qi.Amazonka                (runAmazonka)


createStack
  :: Text
  -> AWS ()
createStack name =
  void . send $ CF.createStack name
            & csTemplateURL ?~ T.concat ["https://s3.amazonaws.com/", name, "/cf.json"]
            & csCapabilities .~ [CapabilityNamedIAM]


updateStack
  :: Text
  -> AWS ()
updateStack name =
  void . send $ CF.updateStack name
            & usTemplateURL ?~ T.concat ["https://s3.amazonaws.com/", name, "/cf.json"]
            & usCapabilities .~ [CapabilityNamedIAM]


deleteStack
  :: Text
  -> AWS ()
deleteStack name =
  void . send $ CF.deleteStack name
                  & dsRetainResources .~ []


data StackDescription = StackDescription {
    sdStatus  :: Text
  , sdOutputs :: [(Text, Text)]
  } deriving (Generic, Show)
instance ToJSON StackDescription


describeStack
 :: Text
 -> AWS StackDescription
describeStack name = do
  r <- send $ CF.describeStacks
                & dStackName ?~ name
  case listToMaybe $ r^.dsrsStacks of
    Just stack ->
      return $ StackDescription {
          sdStatus = T.pack . show $ stack^.sStackStatus
        , sdOutputs = map (\o -> (fromJust $ o^.oOutputKey, fromJust $ o^.oOutputValue)) $ stack^.sOutputs
        }
    Nothing ->
      panic "Error: no stack description was returned"


waitOnStackCreated :: Text -> IO ()
waitOnStackCreated name = waitOnStackStatus name SSCreateComplete False

waitOnStackUpdated :: Text -> IO ()
waitOnStackUpdated name = waitOnStackStatus name SSUpdateComplete False

waitOnStackDeleted :: Text -> IO ()
waitOnStackDeleted name = waitOnStackStatus name SSDeleteComplete True

waitOnStackStatus
  :: Text
  -> StackStatus
  -> Bool
  -> IO ()
waitOnStackStatus name status absentOk =do
  stacks <- filter ((==name) . fst)
            . map (\ss -> (ss^.ssStackName, ss^.ssStackStatus))
            . (^.lsrsStackSummaries)
            <$> runAmazonka (send CF.listStacks)
  case listToMaybe stacks of
    Just (_, s) | s == status -> return ()
    Just (_, _) -> loop -- wait for the stack state to change
    Nothing -> if absentOk -- no mention of the stack in the log
                then return () -- it's fine, don't wait any longer
                else loop -- keep waiting for the stack to appear in the log
    where
      loop = waitASecond >> waitOnStackStatus name status absentOk
      waitASecond = threadDelay 10000000
