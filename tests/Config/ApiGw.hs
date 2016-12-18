{-# LANGUAGE OverloadedStrings #-}

module Config.ApiGw where

import           Control.Lens
import           Data.Default                (def)
import           Test.Tasty.Hspec

import           Qi.Program.Config.Interface (ConfigProgram, api, apiResource)

import           Config                      (getConfig, getOutputs,
                                              getTemplate)
import           Util


configProgram :: ConfigProgram ()
configProgram = do
  api "world" >>= \world ->
    apiResource "things" world >>= \things -> return ()
  return ()


spec :: Spec
spec = parallel $
  describe "Api" $ do
    it "outputs api url" $
      outputs `shouldContainKey` "worldApiURL"

  where
    outputs = getOutputs . getTemplate $ getConfig configProgram


