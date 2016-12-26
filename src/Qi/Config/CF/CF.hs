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
import qualified Qi.Config.AWS.CF.Accessors     as CF
import qualified Qi.Config.AWS.Lambda.Accessors as Lambda


toResources config = Resources . map toResource $ CF.getAll config
  where
    toResource custom@Custom{_cLbdId} =
      resource resName $
        CloudFormationCustomResourceProperties $
        cloudFormationCustomResource
          (GetAtt lbdLogicalName "Arn")

      where
        resName = CF.getLogicalName custom config
        lbdLogicalName = Lambda.getLogicalNameFromId (custom^.cLbdId) config


toOutputs config =
  Outputs . foldMap toCustomOutputs $ CF.getAll config

  where

    toCustomOutputs custom = [
        output (T.concat [lname, "UserPoolId"])
          upid
          & description ?~ "UserPoolId"
      , output (T.concat [lname, "UserPoolClientId"])
          upcid
          & description ?~ "UserPoolClientId"
      , output (T.concat [lname, "IdentityPoolId"])
          ipid
          & description ?~ "IdentityPoolId"
      ]

      where
        lname = CF.getLogicalName custom config

        upid  = GetAtt lname "UserPoolId"
        upcid = GetAtt lname "UserPoolClientId"
        ipid  = GetAtt lname "IdentityPoolId"

