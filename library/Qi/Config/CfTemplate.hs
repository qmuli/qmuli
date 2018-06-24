{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CfTemplate (render) where

import qualified Data.ByteString.Lazy    as LBS
import           Protolude
import           Qi.Config.AWS
import qualified Qi.Config.Render.ApiGw  as ApiGw
import qualified Qi.Config.Render.CF     as CF
import qualified Qi.Config.Render.CW     as CW
import qualified Qi.Config.Render.DDB    as DDB
import qualified Qi.Config.Render.Lambda as Lambda
import qualified Qi.Config.Render.Role   as Role
import qualified Qi.Config.Render.S3     as S3
import qualified Qi.Config.Render.SQS    as SQS
import           Stratosphere


render
  :: Config
  -> LBS.ByteString
render config = encodeTemplate $
  template
    (toResources config)
    & templateDescription ?~ "Example"
    & templateFormatVersion ?~ "2010-09-09"
    & templateOutputs ?~ toOutputs config

toResources
  :: Config
  -> Resources
toResources config = mconcat [
    S3.toResources config
  , Role.toResources config
  , Lambda.toResources config
  , ApiGw.toResources config
  , DDB.toResources config
  , SQS.toResources config
  , CF.toResources config
  , CW.toResources config
  ]

toOutputs
  :: Config
  -> Outputs
toOutputs config = mconcat [
    ApiGw.toOutputs config
  , CF.toOutputs config
  ]
