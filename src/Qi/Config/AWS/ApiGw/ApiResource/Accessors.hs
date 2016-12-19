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

getApiResourceLogicalName
  :: ApiResource
  -> Text
getApiResourceLogicalName apir = T.concat [makeAlphaNumeric $ apir^.arName, "ApiResource"]

getApiResourceById
  :: ApiResourceId
  -> Config
  -> ApiResource
getApiResourceById arid config =
  case SHM.lookup arid arMap of
    Just ar -> ar
    Nothing -> error $ "Could not reference api resource with id: " ++ show arid
  where
    arMap = config^.apiGwConfig.acApiResources

getApiChildren
  :: Either ApiId ApiResourceId
  -> Config
  -> [ApiResourceId]
getApiChildren rid config = SHM.lookupDefault [] rid $ config^.apiGwConfig.acApiResourceDeps




