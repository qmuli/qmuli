{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF (render) where

import           Data.Aeson           (Value (Array), object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Stratosphere         hiding (name)

import           Qi.Config.AWS
import qualified Qi.Config.CF.Api     as Api
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

toResources
  :: Config
  -> Resources
toResources config = mconcat [
      S3.toResources config
    , Role.toResources config
    , Lambda.toResources config
    , Api.toResources config
    ]


