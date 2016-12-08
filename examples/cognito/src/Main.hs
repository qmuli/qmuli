{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                hiding (view, (.=))
import           Control.Monad               (void)
import           Data.Aeson                  hiding (decode)
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Web.JWT                     (claims, decode)

import           Qi                          (withConfig)
import           Qi.Config.AWS.Api           (ApiEvent, ApiVerb (Get), aeParams,
                                              rpHeaders)
import           Qi.Program.Config.Interface (ConfigProgram, api, apiAuthorizer,
                                              apiMethodLambda, apiResource,
                                              customResource)
import           Qi.Program.Lambda.Interface (LambdaProgram)
import           Qi.Util                     (argumentsError, successString)
import           Qi.Util.Cognito             (cognitoPoolProviderLambda)

main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      cognito <- customResource "cognitoPoolProvider" $
              cognitoPoolProviderLambda "MyIdentityPool" "MyUserPool" "MyClient"


      api "world" >>= \world -> do
        auth <- apiAuthorizer "myAuth" cognito world

        apiResource "secure" world >>= \secure -> do
          apiMethodLambda "hello" Get secure (Just auth) greetLambda

      return ()


    greetLambda
      :: ApiEvent
      -> LambdaProgram ()
    greetLambda event = do
      withJwt event $ \jwt ->
        successString $ "jwt contents: " ++ show (decode jwt)
        {- successString $ "jwt: " ++ T.unpack jwt -}



withJwt event f = case SHM.lookup "Authorization" $ event^.aeParams.rpHeaders of
  Just x -> f x
  Just unexpected ->
    argumentsError $ "unexpected header parameter: " ++ show unexpected
  Nothing ->
    argumentsError "expected header 'Authorization' was not found"
