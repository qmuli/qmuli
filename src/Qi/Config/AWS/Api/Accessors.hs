{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Api.Accessors where

import           Control.Lens
import           Data.Char            (isAlphaNum)
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromJust)
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
getApiById aid = fromJust . SHM.lookup aid . (^.apiConfig.acApis)


getApiResourceCFResourceName
  :: ApiResource
  -> Text
getApiResourceCFResourceName apir = T.concat [makeAlphaNumeric $ apir^.arName, "ApiResource"]

getApiResourceById
  :: ApiResourceId
  -> Config
  -> ApiResource
getApiResourceById arid = fromJust . SHM.lookup arid . (^.apiConfig.acApiResources)


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



getApiChildren
  :: Either ApiId ApiResourceId
  -> Config
  -> [ApiResourceId]
getApiChildren rid config = SHM.lookupDefault [] rid $ config^.apiConfig.acApiDeps


insertApi
  :: Api
  -> (ApiId, (ApiConfig -> ApiConfig))
insertApi api = (aid, insertIdToApi . insertIdToApiDeps)
  where
    insertIdToApi = acApis %~ SHM.insert aid api
    insertIdToApiDeps = acApiDeps %~ SHM.insert (Left aid) []

    aid = ApiId $ hash api
    aname = api ^. aName

insertApiResource
  :: ApiResource
  -> (ApiResourceId, (ApiConfig -> ApiConfig))
insertApiResource apiResource = (arid, insertIdToApiResource . insertIdToApiDeps)
  where
    insertIdToApiResource = acApiResources %~ SHM.insert arid apiResource
    insertIdToApiDeps = acApiDeps %~ SHM.unionWith (++) (SHM.singleton (apiResource^.arParent) [arid])

    arid = ApiResourceId $ hash apiResource
    arname = apiResource^.arName
