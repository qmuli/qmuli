{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Util.Cognito where

import           Control.Lens                                             hiding
                                                                           (view,
                                                                           (.=))
import           Control.Monad.Trans.Either                               (EitherT (..))
import           Data.Aeson                                               hiding (Result)
import qualified Data.ByteString.Char8                                    as BS
import qualified Data.ByteString.Lazy.Char8                               as LBS
import qualified Data.HashMap.Strict                                      as SHM
import           Data.Text                                                (Text)
import qualified Data.Text                                                as T
import           Network.AWS.CognitoIdentity.CreateIdentityPool
import           Network.AWS.CognitoIdentity.DeleteIdentityPool
import           Network.AWS.CognitoIdentity.Types                        (cipClientId,
                                                                           cipProviderName,
                                                                           cognitoIdentityProvider)
import           Network.AWS.CognitoIdentityProvider.CreateUserPool
import           Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import           Network.AWS.CognitoIdentityProvider.DeleteUserPool
import           Network.AWS.CognitoIdentityProvider.Types                (VerifiedAttributeType (Email),
                                                                           upctClientId,
                                                                           uptId)

import           Qi.Config.AWS.CF
import           Qi.Program.Lambda.Interface                              (LambdaProgram,
                                                                           amazonkaSend,
                                                                           http,
                                                                           output)
import           Qi.Util.Api
import           Qi.Util.CustomCFResource

cognitoPoolProviderLambda
  :: Text
  -> Text
  -> Text
  -> CfEventHandler
cognitoPoolProviderLambda identityPoolName userPoolName userPoolClientName =
  customResourceProviderLambda $
    CustomResourceProvider createHandler updateHandler deleteHandler

  where

    createHandler = runEitherT $ do
      upid <- EitherT tryCreateUserPool
      cid  <- EitherT $ tryCreateUserPoolClient upid
      ipid <- EitherT $ tryCreateIdentityPool upid cid
      return $ Result {
          rId = Just $ T.concat [upid, "|", ipid]
        , rAttrs = SHM.fromList [
              ("UserPoolId", String upid)
            , ("UserPoolClientId", String cid)
            , ("IdentityPoolId", String ipid)
            ]
        }


    -- does nothing for now
    updateHandler ids = do
      return $ Right $ Result {
          rId = Just ids
        , rAttrs = SHM.fromList []
        }


    deleteHandler ids = do
      let [upid, ipid] = T.splitOn "|" ids
      runEitherT $ do
        EitherT $ tryDeleteUserPool upid
        EitherT $ tryDeleteIdentityPool ipid
      return . Right $ Result {
          rId = Just ids
        , rAttrs = SHM.fromList []
        }



    tryCreateUserPool = do
      resp <- amazonkaSend $ createUserPool userPoolName
                              & cupAutoVerifiedAttributes .~ [Email]
      case resp^.cuprsResponseStatus of
        200 ->
          return $ case (^.uptId) =<< resp^.cuprsUserPool of
            Just upid ->
              Right upid
            Nothing ->
              Left $ T.concat ["Error: No User Pool id was returned. Response was: ", T.pack $ show resp]

        unexpected -> do
          -- TODO: need to log error here
          return . Left $ T.concat ["Error: unexpected response status: ", T.pack $ show unexpected, ", complete response: ", T.pack $ show resp]

    tryCreateUserPoolClient upid = do
      resp <- amazonkaSend $ createUserPoolClient upid userPoolClientName
                              & cupcGenerateSecret ?~ False
      case resp^.cupcrsResponseStatus of
        200 ->
          return $ case (^.upctClientId) =<< resp^.cupcrsUserPoolClient of
            Just cid ->
              Right cid
            Nothing ->
              Left $ T.concat ["Error: No User Pool Client id was returned. Response was: ", T.pack $ show resp]

        unexpected -> do
          -- TODO: need to log error here
          return . Left $ T.concat ["Error: unexpected response status: ", T.pack $ show unexpected, ", complete response: ", T.pack $ show resp]




    tryCreateIdentityPool upid cid = do
      let userPoolProvider = cognitoIdentityProvider
            & cipProviderName ?~ T.concat ["cognito-idp.us-east-1.amazonaws.com/", upid]
            & cipClientId ?~ cid


      resp <- amazonkaSend $ createIdentityPool identityPoolName False
        & cipCognitoIdentityProviders .~ [ userPoolProvider ]
      return . Right $ resp^.ipIdentityPoolId


    tryDeleteUserPool upid = do
      -- for some reason no status is returned:
      -- https://hackage.haskell.org/package/amazonka-cognito-idp-1.4.4/docs/Network-AWS-CognitoIdentityProvider-DeleteUserPool.html
      resp <- amazonkaSend $ deleteUserPool upid
      return $ Right upid


    tryDeleteIdentityPool ipid = do
      -- for some reason no status is returned:
      -- https://hackage.haskell.org/package/amazonka-cognito-identity-1.4.4/docs/Network-AWS-CognitoIdentity-DeleteIdentityPool.html
      resp <- amazonkaSend $ deleteIdentityPool ipid
      return $ Right ipid
