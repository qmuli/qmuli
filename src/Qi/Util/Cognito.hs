{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Util.Cognito where

import           Control.Lens                                             hiding
                                                                           (view,
                                                                           (.=))
import           Control.Monad.Trans.Except                               (ExceptT (..),
                                                                           runExceptT)
import           Data.Aeson                                               hiding (Result)
import qualified Data.ByteString.Char8                                    as BS
import qualified Data.ByteString.Lazy.Char8                               as LBS
import qualified Data.HashMap.Strict                                      as SHM
import           Data.Text                                                (Text)
import qualified Data.Text                                                as T
import           Data.Text.Encoding                                       (decodeUtf8)
import           Network.AWS.CognitoIdentity.CreateIdentityPool
import           Network.AWS.CognitoIdentity.DeleteIdentityPool
import           Network.AWS.CognitoIdentity.SetIdentityPoolRoles
import           Network.AWS.CognitoIdentity.Types                        (cipClientId,
                                                                           cipProviderName,
                                                                           cognitoIdentityProvider)
import           Network.AWS.CognitoIdentityProvider.CreateUserPool
import           Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import           Network.AWS.CognitoIdentityProvider.DeleteUserPool
import           Network.AWS.CognitoIdentityProvider.Types                (VerifiedAttributeType (Email),
                                                                           upctClientId,
                                                                           uptId)
import           Network.AWS.IAM                                          hiding (String)
import           Protolude

import           Qi.Config.AWS.CF
import           Qi.Program.Lambda.Interface                              (LambdaProgram,
                                                                           amazonkaSend,
                                                                           getAppName,
                                                                           http,
                                                                           say)
import           Qi.Util.ApiGw
import           Qi.Util.CustomCFResource


-- to satisfy the unreasonable and inconsistent demands for AWS resource naming
dashToUnderscore
  :: Text
  -> Text
dashToUnderscore = T.replace "-" "_"

cognitoPoolProviderLambda
  :: CfEventHandler
cognitoPoolProviderLambda =
  customResourceProviderLambda $
    CustomResourceProvider createHandler updateHandler deleteHandler

  where
    createHandler = do
      say "creating the custom Cognito resource..."
      runExceptT $ do

        upid <- ExceptT tryCreateUserPool
        cid  <- ExceptT $ tryCreateUserPoolClient upid
        (ipid, arid) <- ExceptT $ tryCreateIdentityPool upid cid
        return $ Result {
            rId = Just $ T.intercalate "|" [upid, ipid, arid]
          , rAttrs = SHM.fromList [
                ("UserPoolId", String upid)
              , ("UserPoolClientId", String cid)
              , ("AuthenticatedRoleId", String arid)
              , ("IdentityPoolId", String ipid)
              ]
          }


    -- does nothing for now
    updateHandler ids = do
      say "updating the custom Cognito resource..."
      return $ Right $ Result {
          rId = Just ids
        , rAttrs = SHM.fromList []
        }


    deleteHandler ids = do
      say "deleting the custom Cognito resource..."
      let [upid, ipid, arid] = T.splitOn "|" ids
      runExceptT $ do
        ExceptT $ tryDeleteUserPool upid
        ExceptT $ tryDeleteIdentityPool ipid
        ExceptT $ tryDeleteAuthenticatedRole arid
      return . Right $ Result {
          rId = Just ids
        , rAttrs = SHM.fromList []
        }



    tryCreateUserPool = do
      say "creating user pool..."
      name <- (`T.append` "_UserPool") . dashToUnderscore <$> getAppName

      resp <- amazonkaSend $ createUserPool name
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
      say "creating user pool client..."
      name <- (`T.append` "_UserPoolClient") . dashToUnderscore <$> getAppName
      resp <- amazonkaSend $ createUserPoolClient upid name
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


      say "creating identity pool..."
      appName <- dashToUnderscore <$> getAppName
      resp <- amazonkaSend $ createIdentityPool (T.append appName "_IdentityPool") False
        & cipCognitoIdentityProviders .~ [ userPoolProvider ]

      let ipid = resp^.ipIdentityPoolId

      let doc = decodeUtf8 . LBS.toStrict . encode $ object [
              ("Version", "2012-10-17")
            , ("Statement", stmnt)
            ]

          stmnt = object [
              ("Effect", "Allow")
            , ("Principal", principal)
            , ("Action", "sts:AssumeRoleWithWebIdentity")
            , ("Condition", condition)
            ]

          condition = object [
              ("StringEquals", object [("cognito-identity.amazonaws.com:aud", String ipid)])
            , ("ForAnyValue:StringLike", object [("cognito-identity.amazonaws.com:amr", "authenticated")])
            ]

          principal = object [ ("Federated", "cognito-identity.amazonaws.com") ]


      say "creating authenticated role..."
      let cognitoAuthenticatedRoleName = T.append appName "_CognitoAuthenticatedRole"
          cognitoAuthenticatedPolicyName = T.append appName "_CognitoAuthenticatedPolicy"
      crr <- amazonkaSend $ createRole cognitoAuthenticatedRoleName doc
      case crr^.crrsResponseStatus of
        200 -> do
          let authenticatedRole = crr^.crrsRole
              authenticatedRoleId = authenticatedRole^.rRoleId
              authenticatedRoleARN = authenticatedRole^.rARN


          let policyDoc = decodeUtf8 . LBS.toStrict . encode $ object [
                  ("Version", "2012-10-17")
                , ("Statement", Array [stmnt', stmnt''])
                ]

              stmnt' = object [
                  ("Effect", "Allow")
                , ("Action", Array [
                      "mobileanalytics:PutEvents"
                    , "cognito-sync:*"
                    , "cognito-identity:*"
                    ]
                  )
                , ("Resource", Array ["*"])
                ]

              stmnt'' = object [
                  ("Effect", "Allow")
                , ("Action", Array [
                      "lambda:*"
                    ]
                  )
                , ("Resource", Array ["*"])
                ]

          say "putting role policy ..."
          prpr <- amazonkaSend $ putRolePolicy cognitoAuthenticatedRoleName cognitoAuthenticatedPolicyName policyDoc


          say "setting identity pool roles..."
          sipr <- amazonkaSend $ setIdentityPoolRoles ipid
                      & siprRoles .~ [("authenticated", authenticatedRoleARN)]

          return $ Right (ipid, authenticatedRoleId)


        unexpected ->
          return . Left $ T.concat ["Error: unexpected response status while creating an authenticated role, complete response: ", T.pack $ show crr]


    tryDeleteUserPool upid = do
      -- for some reason no status is returned:
      -- https://hackage.haskell.org/package/amazonka-cognito-idp-1.4.4/docs/Network-AWS-CognitoIdentityProvider-DeleteUserPool.html
      say $ T.concat ["deleting user pool with id: ", upid, " ..."]
      resp <- amazonkaSend $ deleteUserPool upid
      return $ Right upid


    tryDeleteIdentityPool ipid = do
      -- for some reason no status is returned:
      -- https://hackage.haskell.org/package/amazonka-cognito-identity-1.4.4/docs/Network-AWS-CognitoIdentity-DeleteIdentityPool.html
      say $ T.concat ["deleting identity pool with id: ", ipid, " ..."]
      resp <- amazonkaSend $ deleteIdentityPool ipid
      return $ Right ipid

    tryDeleteAuthenticatedRole arid = do
      appName <- dashToUnderscore <$> getAppName
      let roleName = T.append appName "_CognitoAuthenticatedRole"
          policyName = T.append appName "_CognitoAuthenticatedPolicy"

      -- before deleting a role, all the associated policies must be deleted
      say $ T.concat ["deleting authenticated role policy: ", policyName, " ..."]
      drpr <- amazonkaSend $ deleteRolePolicy roleName policyName

      say $ T.concat ["deleting authenticated role: ", roleName, " ..."]
      drr <- amazonkaSend $ deleteRole roleName
      return $ Right arid
