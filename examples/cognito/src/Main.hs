{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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
import           System.Environment                                 (getArgs,
                                                                     withArgs)

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

import           Custom



main :: IO ()
main = do
  args <- getArgs
  case args of
    (appName:rest) -> withArgs rest $ (T.pack appName) `withConfig` config
    _              -> putStrLn "Please provide a unique application name for your qmulus"

    where
      config :: ConfigProgram ()
      config = do
        void $ customResourceLambda "cognitoIdentityPoolProvider" cognitoUserPoolProviderLambda


      cognitoUserPoolProviderLambda
        :: CfEventHandler
      cognitoUserPoolProviderLambda =
        customResourceProviderLambda $
          CustomResourceProvider createHandler updateHandler deleteHandler

        where

          createHandler responseTemplate = do
            resp <- amazonkaSend $ createUserPool "MyUserPool"
            case resp^.cuprsResponseStatus of
              200 ->
                case (^.uptId) =<< resp^.cuprsUserPool of
                  Just upid ->
                    return $ Right upid
                  Nothing ->
                    return . Left $ T.concat ["Error: No User Pool id was returned. Response was: ", T.pack $ show resp]


              unexpected -> do
                -- TODO: need to log error here
                return . Left $ T.concat ["Error: unexpected response status: ", T.pack $ show unexpected, ", complete response: ", T.pack $ show resp]

            -- does nothing for now
          updateHandler responseTemplate crid = do
            return $ Right crid


          deleteHandler responseTemplate crid = do
            resp <- amazonkaSend $ deleteUserPool crid
            return $ Right crid












