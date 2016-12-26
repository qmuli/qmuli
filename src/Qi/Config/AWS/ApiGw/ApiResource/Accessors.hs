{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.ApiGw.ApiResource.Accessors where


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
  -> Text
getLogicalName apir = T.concat [makeAlphaNumeric $ apir^.arName, "ApiResource"]

getById
  :: ApiResourceId
  -> Config
  -> ApiResource
getById arid config =
  case SHM.lookup arid arMap of
    Just ar -> ar
    Nothing -> error $ "Could not reference api resource with id: " ++ show arid
  where
    arMap = config^.apiGwConfig.acApiResources

getChildren
  :: Either ApiId ApiResourceId
  -> Config
  -> [ApiResourceId]
getChildren rid config = SHM.lookupDefault [] rid $ config^.apiGwConfig.acApiResourceDeps




