{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                          hiding (view, (.=))
import           Control.Monad                         (void)
import           Data.Aeson                            hiding (decode)
import           Data.Default                          (def)
import qualified Data.HashMap.Strict                   as SHM
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Web.JWT                               (claims, decode)

import           Qi                                    (withConfig)
import           Qi.Config.AWS.ApiGw                   (ApiMethodEvent,
                                                        ApiVerb (Get), aeParams,
                                                        rpHeaders)
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ampAuthId)
import           Qi.Program.Config.Interface           (ConfigProgram, api,
                                                        apiAuthorizer,
                                                        apiMethodLambda,
                                                        apiResource,
                                                        customResource)
import           Qi.Program.Lambda.Interface           (LambdaProgram)
import           Qi.Util                               (argumentsError,
                                                        successString)
import           Qi.Util.Cognito                       (cognitoPoolProviderLambda)

main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      cognito <- customResource "cognitoPoolProvider"
              (cognitoPoolProviderLambda "MyIdentityPool" "MyUserPool" "MyClient") def


      api "world" >>= \world -> do
        authId <- apiAuthorizer "myAuth" cognito world

        apiResource "secure" world >>= \secure -> do
          apiMethodLambda "hello" Get secure (def & ampAuthId ?~ authId) greetLambda def

      return ()


    greetLambda
      :: ApiMethodEvent
      -> LambdaProgram ()
    greetLambda event = do
      withJwt event $ \jwt ->
        successString $ "jwt contents: " ++ show (decode jwt)
        {- successString $ "jwt: " ++ T.unpack jwt -}



withJwt event f = case SHM.lookup "Authorization" $ event^.aeParams.rpHeaders of
  Just x -> f x
  Nothing ->
    argumentsError "expected header 'Authorization' was not found"
