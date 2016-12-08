{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Config.AWS.Api.Event where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types

import           Qi.Config.AWS
import           Qi.Config.AWS.Api


parse
  :: Value
  -> Config
  -> Parser ApiEvent
parse (Object e) _ = ApiEvent <$> paramsParser <*> bodyParser
  where
    paramsParser :: Parser RequestParams
    paramsParser = do
      params  <- e .: "params"
      path    <- params .: "path"
      headers <- params .: "header"
      return $ RequestParams path headers

    bodyParser = jsonParser <|> plainTextParser <|> pure EmptyBody
    jsonParser = JsonBody <$> e .: "body-json"
    plainTextParser = PlainTextBody <$> e .: "body-plaintext"

parse _ _ =
  fail "event must be a json object"
