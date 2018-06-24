{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Config.AWS.ApiGw.ApiMethod.Event where

import           Data.Aeson
import           Data.Aeson.Types
import           Protolude

import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw (ApiMethodEvent (..), RequestBody (..),
                                      RequestParams (..))


parse
  :: Config
  -> Value
  -> Parser ApiMethodEvent
parse _ = withObject "ApiMethodEvent" eventObjectParser
  where
    eventObjectParser o = ApiMethodEvent <$> paramsParser <*> bodyParser

      where
        paramsParser :: Parser RequestParams
        paramsParser = do
          params  <- o .: "params"
          path    <- params .: "path"
          headers <- params .: "header"
          return $ RequestParams path headers

        bodyParser = jsonParser <|> plainTextParser <|> pure EmptyBody
        jsonParser = JsonBody <$> o .: "body-json"
        plainTextParser = PlainTextBody <$> o .: "body-plaintext"

