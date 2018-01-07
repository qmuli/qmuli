{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Qi.Config.AWS.CF
import           Qi.Program.Lambda.Interface (LambdaProgram, amazonkaSend, http)
import           Qi.Util                     (respond)


type CfEventHandler = CfEvent -> LambdaProgram LBS.ByteString

data CfStatus = CfSuccess | CfFailed
instance ToJSON CfStatus where
  toJSON CfSuccess = String "SUCCESS"
  toJSON CfFailed  = String "FAILED"


data Response = Response {
    rStatus             :: CfStatus
  , rReason             :: Text
  , rPhysicalResourceId :: Maybe Text
  , rStackId            :: Text
  , rRequestId          :: Text
  , rLogicalResourceId  :: Text
  , rData               :: Object
  } deriving Generic

data Result = Result {
    rId    :: Maybe Text
  , rAttrs :: Object
  }

instance ToJSON Response where
  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = drop 1 }


data CustomResourceProvider = CustomResourceProvider {
    onCreate :: LambdaProgram (Either Text Result)
  , onUpdate :: Text -> LambdaProgram (Either Text Result)
  , onDelete :: Text -> LambdaProgram (Either Text Result)
  }

customResourceProviderLambda
  :: CustomResourceProvider
  -> CfEventHandler
customResourceProviderLambda CustomResourceProvider{..} event = do
  let responseTemplate = Response{
        rStatus             = CfSuccess
      , rReason             = "undefined"
      , rStackId            = event^.cfeStackId
      , rRequestId          = event^.cfeRequestId
      , rLogicalResourceId  = event^.cfeLogicalResourceId
      , rPhysicalResourceId = Nothing
      , rData               = SHM.fromList []
      }


  eitherResult <- case event of
    CfEventCreate{} -> onCreate
    CfEventUpdate{} -> onUpdate $ event^.cfePhysicalResourceId
    -- NOTE: (looks like) the handler should return the same PhysicalResourceId that was
    -- passed to it. Otherwise CF thinks that the resource have not been deleted and
    -- gets stuck
    CfEventDelete{} -> onDelete $ event^.cfePhysicalResourceId




  let parsedRequest       = parseRequest_ . T.unpack $ event^.cfeResponseURL
      encodedResponse     = encode response
      encodedResponseSize = LBS.length encodedResponse
      response            = case eitherResult of
                              Left err ->
                                responseTemplate{
                                    rStatus = CfFailed
                                  , rReason = err
                                  }
                              Right Result{rId, rAttrs} ->
                                responseTemplate{
                                    rStatus = CfSuccess
                                  , rPhysicalResourceId = rId
                                  , rData = rAttrs
                                  }
      request             = parsedRequest{
                                method          = "PUT"
                              , requestBody     = RequestBodyLBS encodedResponse
                              , requestHeaders  = [
                                    ("content-type", "")
                                  , ("content-length", BS.pack $ show encodedResponseSize)
                                  ]
                              }


  -- assume successfully written response to S3 object
  responseResp <- http request tlsManagerSettings

  respond 200 . String . T.pack $ show responseResp
