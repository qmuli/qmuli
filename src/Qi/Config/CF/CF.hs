{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.CF (toResources) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (S3Bucket, name)

import           Qi.Config.AWS
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CF.Accessors
import           Qi.Config.AWS.Lambda.Accessors


toResources config = Resources . map toCustomRes $ getAllCustoms config
  where
    toCustomRes custom@Custom{_cLbdId} =
      resource resName $
        CloudFormationCustomResourceProperties $
        cloudFormationCustomResource
          (GetAtt lbdResName "Arn")

      where
        resName = getCustomCFResourceName custom config
        lbdResName = getLambdaCFResourceNameFromId (custom^.cLbdId) config

