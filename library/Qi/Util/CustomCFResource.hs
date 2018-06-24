{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-custom-resources.html

module Qi.Util.CustomCFResource where

import           Control.Lens                hiding (view, (.=))
import           Data.Aeson                  hiding (Result)
import           Data.Aeson.Types            (fieldLabelModifier, typeMismatch)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import qualified Data.HashMap.Strict         as SHM
import qualified Data.Text                   as T
import           GHC.Generics

import           Network.HTTP.Client         (Request (..), RequestBody (..),
                                              parseRequest_)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Protolude
import           Qi.AWS.Types
import           Qi.Config.AWS.CF
import           Qi.Program.Lambda.Interface (LambdaProgram, amazonkaSend, http)



type CfEventHandler = CfEvent -> LambdaProgram LBS.ByteString

data CustomResourceStatus = CustomResourceSuccess | CustomResourceFailure
instance ToJSON CustomResourceStatus where
  toJSON CustomResourceSuccess = String "SUCCESS"
  toJSON CustomResourceFailure = String "FAILED"


data Response = Response {
    rStatus             :: CustomResourceStatus
  , rReason             :: Text
  , rPhysicalResourceId :: Maybe CompositeResourceId
  , rStackId            :: Arn
  , rRequestId          :: Text
  , rLogicalResourceId  :: LogicalResourceId
  , rData               :: Object
  } deriving Generic

data Result = Result {
    rId    :: Maybe CompositeResourceId
  , rAttrs :: Object
  }

instance ToJSON Response where
  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = drop 1 }


data CustomResourceProvider = CustomResourceProvider {
    onCreate :: LambdaProgram (Either Text Result)
  , onUpdate :: CompositeResourceId -> LambdaProgram (Either Text Result)
  , onDelete :: CompositeResourceId -> LambdaProgram (Either Text Result)
  }


customResourceProviderLambda
  :: CustomResourceProvider
  -> CfEventHandler
customResourceProviderLambda CustomResourceProvider{..} event = do
  let responseTemplate = Response{
        rStatus             = CustomResourceSuccess
      , rReason             = "undefined"
      , rStackId            = event ^. cfeStackId
      , rRequestId          = event ^. cfeRequestId
      , rLogicalResourceId  = event ^. cfeLogicalResourceId
      , rPhysicalResourceId = Nothing
      , rData               = SHM.empty
      }

  eitherResult <- case event of
    CfEventCreate{} -> onCreate
    CfEventUpdate{ _cfePhysicalResourceId } -> onUpdate _cfePhysicalResourceId
    -- NOTE: (looks like) the handler should return the same PhysicalResourceId that was
    -- passed to it. Otherwise CF thinks that the resource have not been deleted and
    -- gets stuck
    CfEventDelete{ _cfePhysicalResourceId } -> onDelete _cfePhysicalResourceId


  let parsedRequest       = parseRequest_ . toS $ event ^. cfeResponseURL
      encodedResponse     = encode response
      encodedResponseSize = LBS.length encodedResponse
      response            = case eitherResult of
                              Left err ->
                                responseTemplate{
                                    rStatus = CustomResourceFailure
                                  , rReason = err
                                  }
                              Right Result{rId, rAttrs} ->
                                responseTemplate{
                                    rStatus             = CustomResourceSuccess
                                  , rPhysicalResourceId = rId
                                  , rData               = rAttrs
                                  }
      request             = parsedRequest{
                                method          = "PUT"
                              , requestBody     = RequestBodyLBS encodedResponse
                              , requestHeaders  = [
                                    ("content-type", "")
                                  , ("content-length", show encodedResponseSize)
                                  ]
                              }


  -- assume successfully written response to S3 object
  responseResp <- http tlsManagerSettings request

  pure $ show responseResp

