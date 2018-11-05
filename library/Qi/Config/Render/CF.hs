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
    toResource customResource@CfCustomResource{ _cLbdId } =
      resource (unLogicalName $ getLogicalName config customResource) $
        CloudFormationCustomResourceProperties $
        cloudFormationCustomResource $
          GetAtt (unLogicalName lbdName) "Arn"

      where
        lbdName = getLogicalNameFromId config _cLbdId

toOutputs :: Config -> Outputs
toOutputs config =
  Outputs . foldMap toCustomOutputs $ getAll config

  where
    toCustomOutputs
      :: CfCustomResource
      -> [Output]
    toCustomOutputs custom = [
        output (lname <> "UserPoolId")
          userPoolId
          & outputDescription ?~ "UserPoolId"
      , output (lname <> "UserPoolClientId")
          userPoolClientId
          & outputDescription ?~ "UserPoolClientId"
      , output (lname <> "IdentityPoolId")
          idPoolId
          & outputDescription ?~ "IdentityPoolId"
      ]

      where
        lname = unLogicalName $ getLogicalName config custom

        userPoolId        = GetAtt lname "UserPoolId"
        userPoolClientId  = GetAtt lname "UserPoolClientId"
        idPoolId          = GetAtt lname "IdentityPoolId"

