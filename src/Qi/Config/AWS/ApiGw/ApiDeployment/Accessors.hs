{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.ApiGw.ApiDeployment.Accessors where


import           Control.Lens
import           Data.Char            (isAlphaNum)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.Identifier


getLogicalName
  :: Api
  -> Text
getLogicalName api = T.concat [api^.aName, "ApiDeployment"]



