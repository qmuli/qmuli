{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Lens
import           Data.Aeson       (Value)
import           Data.Aeson.Lens  (key)
import           Data.Maybe       (fromJust, isJust)
import           Data.Text        (Text)
import           Test.Tasty.Hspec (shouldBe)


shouldContainKey :: Value -> Text -> IO ()
shouldContainKey value tag = isJust (value ^? key tag) `shouldBe` True


