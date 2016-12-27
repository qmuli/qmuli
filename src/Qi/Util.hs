{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Util where

import           Data.Aeson                  (Result (Error, Success),
                                              Value (Number, Object, String),
                                              encode, object)
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.HashMap.Strict         (HashMap)
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Qi.Program.Lambda.Interface (CompleteLambdaProgram)


success
  :: Value
  -> CompleteLambdaProgram
success v =
  respond 200 $ case v of
    Object _ -> v
    String _ -> object [ ("message", v) ]

created = respond 201

argumentsError = respond 400 . String . T.pack
notFoundError = respond 404 . String . T.pack
internalError = respond 500 . String . T.pack

respond
  :: Int
  -> Value
  -> CompleteLambdaProgram
respond status content =
  return . encode $ object [
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
  -> CompleteLambdaProgram
  -> CompleteLambdaProgram
withSuccess code f =
  case code of
    200         -> f
    unexpected  ->
      internalError $ "Error: unexpected response status: " ++ show unexpected





