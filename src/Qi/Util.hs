{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Util where

import           Data.Aeson                  (Result (Error, Success),
                                              Value (Number, String), encode,
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

result
  :: (String -> b)
  -> (a -> b)
  -> Result a
  -> b
result f g r =
  case r of
    Error err -> f err
    Success x -> g x

withSuccess
  :: Int
  -> LambdaProgram ()
  -> LambdaProgram ()
withSuccess code f =
  case code of
    200         -> f
    unexpected  ->
      internalError $ "Error: unexpected response status: " ++ show unexpected





