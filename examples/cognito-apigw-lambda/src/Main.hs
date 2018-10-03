{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                          hiding (view, (.=))
import           Control.Monad                         (void)
import           Data.Aeson                            (Value (String))
import           Data.Default                          (def)
import qualified Data.HashMap.Strict                   as SHM
import qualified Data.Text                             as T
import           Protolude
import           Web.JWT                               (claims, decode)

import           Qi                                    (withConfig)
import           Qi.Config.AWS.ApiGw                   (ApiVerb (Get), aeParams,
                                                        rpHeaders)
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile (ampAuthId)
import qualified Qi.CustomResource.Cognito             as Cognito
import           Qi.Program.Config.Interface           (ConfigProgram, api,
                                                        apiAuthorizer,
                                                        apiMethodLambda,
                                                        apiResource,
                                                        customResource)
import           Qi.Program.Lambda.Interface           (ApiLambdaProgram, say)


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      cognito <- customResource "cognitoProvider"
              Cognito.providerLambda def


      void $ api "world" >>= \world -> do
        authId <- apiAuthorizer "myAuth" cognito world

        apiResource "secure" world >>= \secure -> do
          apiMethodLambda "hello" Get secure (def & ampAuthId ?~ authId) greetLambda def


    greetLambda
      :: ApiLambdaProgram
    greetLambda event = do
      withJwt event $ \jwt -> do
        say $ T.concat ["jwt contents: ", show (decode jwt)]
        pure "lambda had executed successfully"


withJwt event f = case SHM.lookup "Authorization" $ event ^. aeParams . rpHeaders of
  Just jwt -> f jwt
  Nothing ->
    pure "expected header 'Authorization' was not found"

