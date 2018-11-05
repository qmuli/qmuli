{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.Render.CW (toResources) where

import           Control.Lens
import           Data.Aeson            (Value (Array), object)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.HashMap.Strict   as SHM
import qualified Data.Text             as T
import           Protolude             hiding (getAll)
import           Stratosphere

import           Qi.Config.AWS
import           Qi.Config.AWS.CW
import qualified Qi.Config.Render.Role as Role


toResources :: Config -> Resources
toResources config = Resources . map toResource $ getAll config
  where
    toResource rule =
      resource (unLogicalName $ getLogicalName config rule) $
        EventsRuleProperties $
        eventsRule
        & erName ?~ (Literal $ unPhysicalName name)
        & erScheduleExpression ?~ Literal (rule ^. cerProfile . csepSchedule)
        & erState ?~ Literal ENABLED
        & erTargets ?~ [target]

      where
        target = eventsRuleTarget
          tarn
          (Literal tname)
        tname = T.concat [unPhysicalName name, "Lambda"]
        tarn  = GetAtt (unLogicalName $ getLogicalNameFromId config $ rule ^. cerLbdId) "Arn"
        name  = getPhysicalName config rule
