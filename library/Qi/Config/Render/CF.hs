{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Qi.Config.Render.CF (toResources, toOutputs) where

import           Control.Lens
import           Data.Aeson           (Value (Array), object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Protolude            hiding (getAll)
import           Qi.Config.AWS
import           Qi.Config.AWS.CF
import           Stratosphere         (Output, Outputs (Outputs), ResourceProperties (CloudFormationCustomResourceProperties),
                                       Resources (Resources), Val (GetAtt),
                                       cloudFormationCustomResource, output,
                                       outputDescription, resource)

toResources :: Config -> Resources
toResources config = Resources . map toResource $ getAll config
  where
    toResource custom@Custom{_cLbdId} =
      resource resName $
        CloudFormationCustomResourceProperties $
        cloudFormationCustomResource
          (GetAtt lbdLogicalName "Arn")

      where
        resName = getLogicalName config custom
        lbdLogicalName = getLogicalNameFromId config (custom ^. cLbdId)

toOutputs :: Config -> Outputs
toOutputs config =
  Outputs . foldMap toCustomOutputs $ getAll config

  where
    toCustomOutputs
      :: Custom
      -> [Output]
    toCustomOutputs custom = [
        output (T.concat [lname, "UserPoolId"])
          upid
          & outputDescription ?~ "UserPoolId"
      , output (T.concat [lname, "UserPoolClientId"])
          upcid
          & outputDescription ?~ "UserPoolClientId"
      , output (T.concat [lname, "IdentityPoolId"])
          ipid
          & outputDescription ?~ "IdentityPoolId"
      ]

      where
        lname = getLogicalName config custom

        upid  = GetAtt lname "UserPoolId"
        upcid = GetAtt lname "UserPoolClientId"
        ipid  = GetAtt lname "IdentityPoolId"

