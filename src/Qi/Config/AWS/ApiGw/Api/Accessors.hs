{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.ApiGw.Api.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.Identifier


getLogicalName
  :: Api
  -> Text
getLogicalName api = T.concat [api^.aName, "Api"]

getById
  :: ApiId
  -> Config
  -> Api
getById aid config =
  case SHM.lookup aid aMap of
    Just a  -> a
    Nothing -> error $ "Could not reference api with id: " ++ show aid
  where
    aMap = config^.apiGwConfig.acApis

getAll
  :: Config
  -> [(ApiId, Api)]
getAll config =
  SHM.toList $ config^.apiGwConfig.acApis


