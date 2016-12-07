{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Config.AWS.CF.Event where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types

import           Qi.Config.AWS
import           Qi.Config.AWS.CF


{- parse -}
  {- :: Value -}
  {- -> Config -}
  {- -> Parser CFEvent -}
{- parse (Object e) _ = CFEvent <$> paramsParser <*> bodyParser -}
  {- where -}
    {- paramsParser :: Parser RequestParams -}
    {- paramsParser = do -}
      {- params  <- e .: "params" -}
      {- path    <- params .: "path" -}
      {- return $ RequestParams path -}

    {- bodyParser = jsonParser <|> plainTextParser <|> pure EmptyBody -}
    {- jsonParser = JsonBody <$> e .: "body-json" -}
    {- plainTextParser = PlainTextBody <$> e .: "body-plaintext" -}

{- parse _ _ = -}
  {- fail "event must be a json object" -}
