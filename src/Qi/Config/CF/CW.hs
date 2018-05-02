{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.CW (toResources) where

import           Control.Lens
import           Data.Aeson           (Value (Array), object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as SHM
import qualified Data.Text            as T
import           Protolude            hiding (getAll)
import           Stratosphere

import           Qi.Config.AWS
import           Qi.Config.AWS.CW
import qualified Qi.Config.CF.Role    as Role


toResources :: Config -> Resources
toResources config = Resources . map toResource $ getAll config
  where
    toResource rule =
      resource lname $
        EventsRuleProperties $
        eventsRule
        & erName ?~ (Literal pname)
        & erScheduleExpression ?~ Literal (rule^.cerProfile.csepSchedule)
        & erState ?~ Literal ENABLED
        & erTargets ?~ [target]

      where
        target = eventsRuleTarget
          tarn
          (Literal tname)
        tname = T.concat [pname, "Lambda"]
        tarn  = GetAtt (getLogicalNameFromId config $ rule^.cerLbdId) "Arn"
        lname = getLogicalName config rule
        pname = getPhysicalName config rule
