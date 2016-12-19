{-# LANGUAGE OverloadedStrings #-}

module Config.ApiGw where

import           Control.Lens
import           Data.Default                (def)
import           Test.Tasty.Hspec

import           Qi.Program.Config.Interface (ConfigProgram, api, apiResource)

import           Config                      (getConfig, getOutputs,
                                              getResources, getTemplate)
import           Util


configProgram :: ConfigProgram ()
configProgram = do
  api "world" >>= \world ->
    apiResource "things" world >>= \things ->
      return ()



spec :: Spec
spec = parallel $
  describe "Api" $ do
    context "Outputs" $
      it "has the Api URL" $
        outputs `shouldContainKey` "worldApiURL"

    context "Template" $ do
      it "has the expected Api resource under the correct logical name" $
        resources `shouldContainKey` "worldApi"

      it "has the expected ApiResource resource under the correct logical name" $
        resources `shouldContainKey` "thingsApiResource"

  where
    outputs = getOutputs template
    resources = getResources template

    template = getTemplate $ getConfig configProgram

