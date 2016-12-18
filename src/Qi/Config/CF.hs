{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF (render) where

import qualified Data.ByteString.Lazy as LBS
import           Stratosphere         hiding (name)

import           Qi.Config.AWS
import qualified Qi.Config.CF.ApiGw   as ApiGw
import qualified Qi.Config.CF.CF      as CF
import qualified Qi.Config.CF.DDB     as DDB
import qualified Qi.Config.CF.Lambda  as Lambda
import qualified Qi.Config.CF.Role    as Role
import qualified Qi.Config.CF.S3      as S3


render
  :: Config
  -> LBS.ByteString
render config = encodeTemplate $
  template
    (toResources config)
    & description ?~ "Example"
    & formatVersion ?~ "2010-09-09"
    & outputs ?~ toOutputs config

toResources
  :: Config
  -> Resources
toResources config = mconcat [
    S3.toResources config
  , Role.toResources config
  , Lambda.toResources config
  , ApiGw.toResources config
  , DDB.toResources config
  , CF.toResources config
  ]

toOutputs
  :: Config
  -> Outputs
toOutputs config = mconcat [
    ApiGw.toOutputs config
  , CF.toOutputs config
  ]
