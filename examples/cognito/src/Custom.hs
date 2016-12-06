{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Custom where

import           Control.Lens                hiding (view, (.=))
import           Data.Aeson
import           Data.Aeson.Types            (fieldLabelModifier, typeMismatch)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Generics

import           Network.HTTP.Client         (Request (..), RequestBody (..),
                                              parseRequest_)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)

import           Qi.Config.AWS.CF
import           Qi.Program.Lambda.Interface (LambdaProgram, amazonkaSend, http,
                                              output)
import           Qi.Util.Api


type CfEventHandler = CfEvent -> LambdaProgram ()

data CfStatus = CfSuccess | CfFailed
instance ToJSON CfStatus where
  toJSON CfSuccess = String "SUCCESS"
  toJSON CfFailed  = String "FAILED"


data Response = Response {
    rStatus             :: CfStatus
  , rReason             :: Text
  , rPhysicalResourceId :: Text
  , rStackId            :: Text
  , rRequestId          :: Text
  , rLogicalResourceId  :: Text
  , rData               :: Object
  } deriving Generic

instance ToJSON Response where
  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = drop 1 }


data CustomResourceProvider = CustomResourceProvider {
    onCreate :: Response -> LambdaProgram (Either Text Text)
  , onUpdate :: Response -> Text -> LambdaProgram (Either Text Text)
  , onDelete :: Response -> Text -> LambdaProgram (Either Text Text)
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
      , rPhysicalResourceId = "undefined"
      , rData               = SHM.fromList []
      }


  result <- case event of
    CfEventCreate{} -> onCreate responseTemplate
    CfEventUpdate{} -> onUpdate responseTemplate $ event^.cfePhysicalResourceId
    CfEventDelete{} -> onDelete responseTemplate $ event^.cfePhysicalResourceId




  let parsedRequest       = parseRequest_ . T.unpack $ event^.cfeResponseURL
      encodedResponse     = encode response
      encodedResponseSize = LBS.length encodedResponse
      response            = case result of
                              Left err ->
                                responseTemplate{
                                  rStatus = CfFailed
                                , rReason = err
                                }
                              Right prid ->
                                responseTemplate{rPhysicalResourceId = prid}
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
