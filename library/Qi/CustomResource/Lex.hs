{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.CustomResource.Lex (providerLambda) where

import           Control.Lens                         hiding (view, (.=))
import           Control.Monad.Trans.Except           (ExceptT (..), runExceptT)
import           Data.Aeson                           hiding (Result)
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy.Char8           as LBS
import qualified Data.HashMap.Strict                  as SHM
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Network.AWS.LexModels
import           Protolude
import           Qi.AWS.CF
import           Qi.AWS.Lex
import           Qi.AWS.Types
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CfCustomResource
import           Qi.Config.AWS.CfCustomResource.Types
import           Qi.Program.Lambda.Interface          (CfCustomResourceLambdaProgram,
                                                       LambdaProgram,
                                                       amazonkaSend, getAppName,
                                                       say)


providerLambda
  :: BotName
  -> CfCustomResourceLambdaProgram
providerLambda botName =
  customResourceProviderLambda $
    CustomResourceProvider createHandler updateHandler deleteHandler

  where
    createHandler = do
      say "creating Lex resource..."
      runExceptT $ do
        botDeployment@(LatestBotDeployment _ botChecksum)
          <- ExceptT $ tryCreateBot botName

        pure $ Result {
            rId     = Just $ CustomResourceId [ BotResourceId botDeployment ]
          , rAttrs  = SHM.fromList [
                ("BotName",          toJSON botName)
              , ("BotChecksum",      toJSON botChecksum)
              ]
          }


    -- does nothing for now
    updateHandler ids = do
      say "updating Lex resource..."
      pure . Right $ Result {
          rId = Just ids
        , rAttrs = SHM.fromList []
        }


    deleteHandler ids@(CustomResourceId [ BotResourceId botDeployment ]) = do
      say "deleting Lex resource..."
      -- runExceptT $ do
        -- ExceptT $ tryDeleteUserPool userPoolId
      pure . Right $ Result {
          rId     = Just ids
        , rAttrs  = SHM.fromList []
        }
    deleteHandler unexpectedIds = do
      panic $ "unexpected ids: '" <> show unexpectedIds <> "'"


tryCreateBot
  :: BotName
  -> LambdaProgram (Either Text LatestBotDeployment)
tryCreateBot botName@(BotName bname) = do
  say "creating Lex bot..."

  resp <- amazonkaSend $ putBot bname EnUs False
                          & pbProcessBehavior ?~ Save

  case resp ^. pbrsResponseStatus of
    200 ->
      pure $ case resp ^. pbrsChecksum of
        Just checksum ->
          Right $ LatestBotDeployment
                    botName
                    (BotChecksum checksum)
        Nothing ->
          Left $ "Error: No bot checksum was returned. Response was: '" <> show resp <> "'"

    unexpected -> do
      -- TODO: need to log error here
      pure . Left $ "Error: unexpected response status: '" <> show unexpected <> "', complete response: '" <> show resp <> "'"



{-
    tryCreateUserPool
      :: LambdaProgram (Either Text UserPoolId)
    tryCreateUserPool = do
      say "creating user pool..."
      pure $ Right ()
-}
