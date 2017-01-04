{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                hiding (view, (.=))
import           Control.Monad               (void)
import           Data.Aeson                  (Value (String))
import           Data.Default                (def)
import qualified Data.HashMap.Strict         as SHM
import qualified Data.Text                   as T
import           Web.JWT                     (claims, decode)

import           Qi                          (withConfig)
import           Qi.Program.Config.Interface (ConfigProgram, customResource,
                                              genericLambda)
import           Qi.Program.Lambda.Interface (GenericLambdaProgram, say)
import           Qi.Util                     (argumentsError, success)
import           Qi.Util.Cognito             (cognitoPoolProviderLambda)

main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      cognito <- customResource "cognitoPoolProvider" cognitoPoolProviderLambda def
      void $ genericLambda "greet" greetLambda def


    greetLambda
      :: GenericLambdaProgram
    greetLambda event = do
      say "greetings!"
      success "lambda had executed successfully"




