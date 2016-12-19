{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Qi.Config.CF.CF (toResources, toOutputs) where

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
        resName = getCustomLogicalName custom config
        lbdResName = getLambdaLogicalNameFromId (custom^.cLbdId) config


toOutputs config =
  Outputs . foldMap toCustomOutputs $ getAllCustoms config

  where

    toCustomOutputs custom = [
        output (T.concat [customResName, "UserPoolId"])
          upid
          & description ?~ "UserPoolId"
      , output (T.concat [customResName, "UserPoolClientId"])
          upcid
          & description ?~ "UserPoolClientId"
      , output (T.concat [customResName, "IdentityPoolId"])
          ipid
          & description ?~ "IdentityPoolId"
      ]

      where
        customResName = getCustomLogicalName custom config

        upid  = GetAtt customResName "UserPoolId"
        upcid = GetAtt customResName "UserPoolClientId"
        ipid  = GetAtt customResName "IdentityPoolId"

