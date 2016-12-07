{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad               (void)
import           Data.Aeson
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Qi                          (withConfig)
import           Qi.Program.Config.Interface (ConfigProgram,
                                              customResourceLambda)
import           Qi.Util.Cognito             (cognitoPoolProviderLambda)


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      void . customResourceLambda "cognitoPoolProvider" $
              cognitoPoolProviderLambda "MyIdentityPool" "MyUserPool" "MyClient"









