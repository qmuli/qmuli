{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.CustomResource.Cognito (providerLambda) where

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
import           Network.AWS.CognitoIdentityProvider.Types                (AdvancedSecurityModeType (Off),
                                                                           VerifiedAttributeType (Email),
                                                                           upctClientId,
                                                                           uptId,
                                                                           userPoolAddOnsType)
import           Network.AWS.IAM                                          hiding (String)
import           Protolude
import           Qi.AWS.CF
import           Qi.AWS.Cognito
import           Qi.AWS.Types
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CfCustomResource
import           Qi.Config.AWS.CfCustomResource.Types
import           Qi.Config.AWS.CfCustomResource.Types
import           Qi.Program.Lambda.Interface                              (CfCustomResourceLambdaProgram,
                                                                           LambdaProgram,
                                                                           amazonkaSend,
                                                                           getAppName,
                                                                           say)
import           Qi.Util.ApiGw


-- to satisfy the unreasonable and inconsistent demands for AWS resource naming
dashToUnderscore
  :: Text
  -> Text
dashToUnderscore = T.replace "-" "_"

providerLambda
  :: CfCustomResourceLambdaProgram
providerLambda =
  customResourceProviderLambda $
    CustomResourceProvider createHandler updateHandler deleteHandler

  where
    createHandler = do
      say "creating the custom Cognito resource..."
      runExceptT $ do
        userPoolId              <- ExceptT   tryCreateUserPool
        userPoolClientId        <- ExceptT $ tryCreateUserPoolClient userPoolId
        (idPoolId, authRoleId)  <- ExceptT $ tryCreateIdentityPool userPoolId userPoolClientId
        pure $ Result {
            rId = Just $ CustomResourceId [UserPoolIdResourceId userPoolId, IdPoolIdResourceId idPoolId]
          , rAttrs = SHM.fromList [
                ("UserPoolId",          toJSON userPoolId)
              , ("UserPoolClientId",    toJSON userPoolClientId)
              , ("AuthenticatedRoleId", toJSON authRoleId)
              , ("IdentityPoolId",      toJSON idPoolId)
              ]
          }


    -- does nothing for now
    updateHandler ids = do
      say "updating the custom Cognito resource..."
      pure . Right $ Result {
          rId = Just ids
        , rAttrs = SHM.fromList []
        }


    deleteHandler ids@(CustomResourceId ( [UserPoolIdResourceId userPoolId, IdPoolIdResourceId idPoolId] )) = do
      say "deleting the custom Cognito resource..."
      runExceptT $ do
        ExceptT $ tryDeleteUserPool userPoolId
        ExceptT $ tryDeleteIdentityPool idPoolId
        ExceptT $ tryDeleteAuthenticatedRole
      pure . Right $ Result {
          rId = Just ids
        , rAttrs = SHM.fromList []
        }
    deleteHandler unexpectedIds = do
      panic $ "unexpected ids: '" <> show unexpectedIds <> "'"


    tryCreateUserPool
      :: LambdaProgram (Either Text UserPoolId)
    tryCreateUserPool = do
      say "creating user pool..."
      name <- (<> "_UserPool") . dashToUnderscore <$> getAppName

      resp <- amazonkaSend $ createUserPool name
                              & cupAutoVerifiedAttributes .~ [Email]
                              & cupUserPoolAddOns ?~ userPoolAddOnsType Off

      case resp ^. cuprsResponseStatus of
        200 ->
          pure $ case (^. uptId) =<< resp ^. cuprsUserPool of
            Just upid ->
              Right $ UserPoolId upid
            Nothing ->
              Left $ "Error: No User Pool id was returned. Response was: '" <> show resp <> "'"

        unexpected -> do
          -- TODO: need to log error here
          pure . Left $ T.concat ["Error: unexpected response status: ", T.pack $ show unexpected, ", complete response: ", T.pack $ show resp]


    tryCreateUserPoolClient
      :: UserPoolId
      -> LambdaProgram (Either Text UserPoolClientId)
    tryCreateUserPoolClient (UserPoolId upid) = do
      say "creating user pool client..."
      name <- (<> "_UserPoolClient") . dashToUnderscore <$> getAppName
      resp <- amazonkaSend $ createUserPoolClient upid name
                              & cupcGenerateSecret ?~ False
      case resp ^. cupcrsResponseStatus of
        200 ->
          return $ case (^. upctClientId) =<< resp ^. cupcrsUserPoolClient of
            Just cid ->
              Right $ UserPoolClientId cid
            Nothing ->
              Left $ "Error: No User Pool Client id was returned. Response was: " <> show resp

        unexpected -> do
          -- TODO: need to log error here
          pure . Left $ "Error: unexpected response status: '" <> show unexpected <> "', complete response: '" <> show resp <> "'"


    tryCreateIdentityPool (UserPoolId upid) (UserPoolClientId cid) = do
      let userPoolProvider = cognitoIdentityProvider
            & cipProviderName ?~ "cognito-idp.us-east-1.amazonaws.com/" <> upid
            & cipClientId ?~ cid

      say "creating identity pool..."
      appName <- dashToUnderscore <$> getAppName
      resp <- amazonkaSend $ createIdentityPool (appName <> "_IdentityPool") False
        & cipCognitoIdentityProviders .~ [ userPoolProvider ]

      let idPoolId = resp ^. ipIdentityPoolId

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
              ("StringEquals", object [("cognito-identity.amazonaws.com:aud", String idPoolId)])
            , ("ForAnyValue:StringLike", object [("cognito-identity.amazonaws.com:amr", "authenticated")])
            ]

          principal = object [ ("Federated", "cognito-identity.amazonaws.com") ]


      say "creating authenticated role..."
      let cognitoAuthenticatedRoleName = T.append appName "_CognitoAuthenticatedRole"
          cognitoAuthenticatedPolicyName = T.append appName "_CognitoAuthenticatedPolicy"
      crr <- amazonkaSend $ createRole cognitoAuthenticatedRoleName doc
      case crr ^. crrsResponseStatus of
        200 -> do
          let authRole = crr ^. crrsRole
              authRoleId = authRole ^. rRoleId
              authRoleARN = authRole ^. rARN


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
          sipr <- amazonkaSend $ setIdentityPoolRoles idPoolId
                      & siprRoles .~ [("authenticated", authRoleARN)]

          pure $ Right (IdPoolId idPoolId, AuthRoleId authRoleId)


        unexpected ->
          pure . Left $
            "Error: unexpected response status while creating an authenticated role, complete response: " <> show crr


    tryDeleteUserPool (UserPoolId upid) = do
      -- for some reason no status is returned:
      -- https://hackage.haskell.org/package/amazonka-cognito-idp-1.4.4/docs/Network-AWS-CognitoIdentityProvider-DeleteUserPool.html
      say $ "deleting user pool with id: " <> show upid <> " ..."
      resp <- amazonkaSend $ deleteUserPool upid
      pure $ Right ()



    tryDeleteIdentityPool (IdPoolId ipid) = do
      -- for some reason no status is returned:
      -- https://hackage.haskell.org/package/amazonka-cognito-identity-1.4.4/docs/Network-AWS-CognitoIdentity-DeleteIdentityPool.html
      say $ "deleting identity pool with id: '" <> show ipid <> "' ..."
      resp <- amazonkaSend $ deleteIdentityPool ipid
      pure $ Right ()


    tryDeleteAuthenticatedRole = do
      appName <- dashToUnderscore <$> getAppName
      let roleName    = appName <> "_CognitoAuthenticatedRole"
          policyName  = appName <> "_CognitoAuthenticatedPolicy"

      -- before deleting a role, all the associated policies must be deleted
      say $ "deleting authenticated role policy: " <> policyName <> " ..."
      drpr <- amazonkaSend $ deleteRolePolicy roleName policyName

      say $ "deleting authenticated role: " <> roleName <> " ..."
      drr <- amazonkaSend $ deleteRole roleName

      pure $ Right ()
