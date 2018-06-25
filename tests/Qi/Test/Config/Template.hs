{-# LANGUAGE OverloadedStrings #-}

module Qi.Test.Config.Template where

import           Control.Lens
import           Data.Default                (def)
import           Test.Tasty.Hspec

import           Qi.Program.Config.Interface (ConfigProgram, api, apiResource)

import           Config                      (getConfig, getTemplate)
import           Protolude
import           Util


configProgram :: ConfigProgram ()
configProgram = do
  api "world" >>= \world ->
    apiResource "things" world >>= \things -> return ()
  return ()


spec :: Spec
spec = parallel $
  describe "Template" $ do
    it "contains Resources" $
      template `shouldContainKey` "Resources"

    it "contains Outputs" $
      template `shouldContainKey` "Outputs"

  where
    template = getTemplate $ getConfig configProgram



