{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Api.Accessors where

import           Control.Lens
import           Data.Char            (isAlphaNum)
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Api
import           Qi.Config.Identifier


makeAlphaNumeric = T.filter isAlphaNum

getApiCFResourceName
  :: Api
  -> Text
getApiCFResourceName api = T.concat [api^.aName, "Api"]

getApiById
  :: ApiId
  -> Config
  -> Api
getApiById aid config =
  case SHM.lookup aid aMap of
    Just a  -> a
    Nothing -> error $ "Could not reference api with id: " ++ show aid
  where
    aMap = config^.apiConfig.acApis


getApiAuthorizerCFResourceName
  :: ApiAuthorizer
  -> Text
getApiAuthorizerCFResourceName auth = T.concat [makeAlphaNumeric $ auth^.aaName, "ApiAuthorizer"]


getApiAuthorizerById
  :: ApiAuthorizerId
  -> Config
  -> ApiAuthorizer
getApiAuthorizerById aaid config =
  case SHM.lookup aaid aaMap of
    Just ar -> ar
    Nothing -> error $ "Could not reference api resource with id: " ++ show aaid
  where
    aaMap = config^.apiConfig.acApiAuthorizers


getApiResourceCFResourceName
  :: ApiResource
  -> Text
getApiResourceCFResourceName apir = T.concat [makeAlphaNumeric $ apir^.arName, "ApiResource"]


getApiResourceById
  :: ApiResourceId
  -> Config
  -> ApiResource
getApiResourceById arid config =
  case SHM.lookup arid arMap of
    Just ar -> ar
    Nothing -> error $ "Could not reference api resource with id: " ++ show arid
  where
    arMap = config^.apiConfig.acApiResources


getApiStageCFResourceName
  :: Api
  -> Text
getApiStageCFResourceName api = T.concat [api^.aName, "ApiDeployment"]


getApiMethodCFResourceName
  :: ApiResource
  -> ApiVerb
  -> Text
getApiMethodCFResourceName apir verb = T.concat [makeAlphaNumeric $ apir^.arName, T.pack $ show verb]


getAllApis
  :: Config
  -> [(ApiId, Api)]
getAllApis config = SHM.toList $ config^.apiConfig.acApis

getApiAuthorizers
  :: ApiId
  -> Config
  -> [ApiAuthorizerId]
getApiAuthorizers aid config = SHM.lookupDefault [] aid $ config^.apiConfig.acApiAuthorizerDeps

getApiChildren
  :: Either ApiId ApiResourceId
  -> Config
  -> [ApiResourceId]
getApiChildren rid config = SHM.lookupDefault [] rid $ config^.apiConfig.acApiResourceDeps

