{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-custom-resources.html

module Qi.Config.AWS.CfCustomResource where

import           Control.Lens                         hiding (view, (.=))
import           Data.Aeson                           hiding (Result)
import           Data.Aeson.Types                     (fieldLabelModifier,
                                                       typeMismatch)
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy.Char8           as LBS
import qualified Data.HashMap.Strict                  as SHM
import qualified Data.Text                            as T
import           GHC.Generics
import           Network.HTTP.Client                  (Request (..),
                                                       RequestBody (..),
                                                       parseRequest_)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           Protolude
import           Qi.AWS.CF
import           Qi.AWS.Types
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CfCustomResource.Types
import           Qi.Program.Lambda.Interface          (CfCustomResourceLambdaProgram,
                                                       LambdaProgram,
                                                       amazonkaSend, http)


data CustomResourceProvider = CustomResourceProvider {
    onCreate :: LambdaProgram (Either Text Result)
  , onUpdate :: CompositeResourceId -> LambdaProgram (Either Text Result)
  , onDelete :: CompositeResourceId -> LambdaProgram (Either Text Result)
  }

customResourceProviderLambda
  :: CustomResourceProvider
  -> CfCustomResourceLambdaProgram
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
    CfCustomResourceCreate{} -> onCreate
    CfCustomResourceUpdate{ _cfePhysicalResourceId } -> onUpdate _cfePhysicalResourceId
    -- NOTE: (looks like) the handler should return the same PhysicalResourceId that was
    -- passed to it. Otherwise CF thinks that the resource have not been deleted and
    -- gets stuck
    CfCustomResourceDelete{ _cfePhysicalResourceId } -> onDelete _cfePhysicalResourceId


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

