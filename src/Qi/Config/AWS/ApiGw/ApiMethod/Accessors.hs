{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.ApiGw.ApiMethod.Accessors where

import           Control.Lens
import           Data.Char            (isAlphaNum)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.Identifier


makeAlphaNumeric :: Text -> Text
makeAlphaNumeric = T.filter isAlphaNum

getLogicalName
  :: ApiResource
  -> ApiVerb
  -> Text
getLogicalName apir verb = T.concat [makeAlphaNumeric $ apir^.arName, T.pack $ show verb]


