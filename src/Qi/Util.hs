{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Util where

import           Data.Aeson                  (Value (Number, String), encode,
                                              object)
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Qi.Program.Lambda.Interface (LambdaProgram, output)


success = respond 200
successString s = respond 200 $ object [ ("message", String $ T.pack s) ]
created = respond 201

argumentsError = respond 400 . String . T.pack
notFoundError = respond 404 . String . T.pack
internalError = respond 500 . String . T.pack

respond status content =
  output . encode $ object [
      ("status", Number $ fromIntegral status)
    , ("body", content)
    ]
