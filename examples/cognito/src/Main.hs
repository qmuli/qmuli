{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                          hiding (view, (.=))
import           Control.Monad                         (void)
import           Data.Aeson                            (Value (String))
import           Data.Default                          (def)
import qualified Data.HashMap.Strict                   as SHM
import qualified Data.Text                             as T
import           Web.JWT                               (claims, decode)

import           Qi                                    (withConfig)
import           Qi.Config.AWS.ApiGw                   (ApiVerb (Get), aeParams,
                                                        rpHeaders)
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ampAuthId)
import           Qi.Program.Config.Interface           (ConfigProgram, api,
                                                        apiAuthorizer,
                                                        apiMethodLambda,
                                                        apiResource,
                                                        customResource)
import           Qi.Program.Lambda.Interface           (ApiLambdaProgram, say)
import           Qi.Util                               (argumentsError, success)
import           Qi.Util.Cognito                       (cognitoPoolProviderLambda)

main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      cognito <- customResource "cognitoPoolProvider"
              (cognitoPoolProviderLambda "MyIdentityPool" "MyUserPool" "MyClient") def


      void $ api "world" >>= \world -> do
        authId <- apiAuthorizer "myAuth" cognito world

        apiResource "secure" world >>= \secure -> do
          apiMethodLambda "hello" Get secure (def & ampAuthId ?~ authId) greetLambda def


    greetLambda
      :: ApiLambdaProgram
    greetLambda event = do
      withJwt event $ \jwt -> do
        say $ T.concat ["jwt contents: ", T.pack $ show (decode jwt)]
        success "lambda had executed successfully"




withJwt event f = case SHM.lookup "Authorization" $ event^.aeParams.rpHeaders of
  Just x -> f x
  Nothing ->
    argumentsError "expected header 'Authorization' was not found"
