{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.CW (toResources) where

import           Control.Lens
import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.CW
import qualified Qi.Config.AWS.CW.Accessors     as CW
import qualified Qi.Config.AWS.Lambda.Accessors as Lambda
import qualified Qi.Config.CF.Role              as Role


toResources config = Resources . map toResource $ CW.getAll config
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
        tarn  = GetAtt (Lambda.getLogicalNameFromId (rule^.cerLbdId) config) "Arn"
        lname = CW.getLogicalName rule
        pname = CW.getPhysicalName rule config
