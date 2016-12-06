{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                                       hiding
                                                                     (view,
                                                                     (.=))
import           Control.Monad                                      (forM, void)
import           Data.Aeson
import qualified Data.ByteString.Char8                              as BS
import qualified Data.ByteString.Lazy.Char8                         as LBS
import qualified Data.HashMap.Strict                                as SHM
import           Data.Text                                          (Text)
import qualified Data.Text                                          as T

import           Network.AWS.CognitoIdentityProvider.CreateUserPool
import           Network.AWS.CognitoIdentityProvider.DeleteUserPool
import           Network.AWS.CognitoIdentityProvider.Types          (uptId)
import           Network.HTTP.Client                                (Request (..),
                                                                     RequestBody (..),
                                                                     parseRequest_)
import           Network.HTTP.Client.TLS                            (tlsManagerSettings)

import           Qi                                                 (withConfig)
import           Qi.Config.AWS.Api                                  (ApiEvent (..),
                                                                     ApiVerb (Delete, Get, Post),
                                                                     RequestBody (..),
                                                                     aeBody,
                                                                     aeParams,
                                                                     rpPath)
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.DDB                                  (DdbAttrDef (..),
                                                                     DdbAttrType (..),
                                                                     DdbProvCap (..))
import           Qi.Config.Identifier                               (DdbTableId)
import           Qi.Program.Config.Interface                        (ConfigProgram,
                                                                     customResourceLambda)
import           Qi.Program.Lambda.Interface                        (LambdaProgram,
                                                                     amazonkaSend,
                                                                     http,
                                                                     output)
import           Qi.Util.Api

import           System.Environment                                 (getArgs,
                                                                     withArgs)
import           Types



main :: IO ()
main = do
  args <- getArgs
  case args of
    (appName:rest) -> withArgs rest $ (T.pack appName) `withConfig` config
    _              -> putStrLn "Please provide a unique application name for your qmulus"

    where
      config :: ConfigProgram ()
      config = do
        void $ customResourceLambda "cognitoIdentityPoolProvider" cognitoIdentityPoolProviderLambda

      cognitoIdentityPoolProviderLambda
        :: CfEvent
        -> LambdaProgram ()
      cognitoIdentityPoolProviderLambda event = do
        let responseTemplate = Response{
                rStatus             = CfSuccess
              , rReason             = "undefined"
              , rStackId            = event^.cfeStackId
              , rRequestId          = event^.cfeRequestId
              , rLogicalResourceId  = event^.cfeLogicalResourceId
              , rPhysicalResourceId = "undefined"
              , rData               = SHM.fromList []
              }


        response <- case event of
          CfEventCreate{} -> do
            resp <- amazonkaSend $ createUserPool "MyUserPool"
            case resp^.cuprsResponseStatus of
              200 ->
                case (^.uptId) =<< resp^.cuprsUserPool of
                  Just upid ->
                    return responseTemplate{rPhysicalResourceId = upid}
                  Nothing ->
                    return responseTemplate{
                        rStatus = CfFailed
                      , rReason = T.concat ["Error: No User Pool id was returned. Response was: ", T.pack $ show resp]
                      }


              unexpected -> do
                -- TODO: need to log error here
                return responseTemplate{
                    rStatus = CfFailed
                  , rReason = T.concat ["Error: unexpected response status: ", T.pack $ show unexpected, ", complete response: ", T.pack $ show resp]
                  }
          CfEventUpdate{} -> do
            -- does nothing for now
            return responseTemplate

          CfEventDelete{} -> do
            resp <- amazonkaSend . deleteUserPool $ event^.cfePhysicalResourceId
            return responseTemplate


        let parsedRequest       = parseRequest_ . T.unpack $ event^.cfeResponseURL
            encodedResponse     = encode response
            encodedResponseSize = LBS.length encodedResponse
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





