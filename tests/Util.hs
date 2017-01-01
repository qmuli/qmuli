{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens     (key)
import           Data.Maybe          (fromJust, isJust)
import           Data.Text           (Text)
import           Test.Tasty.Hspec    (shouldBe)


shouldContainKey
  :: Value
  -> Text
  -> IO ()
shouldContainKey template tag = isJust (template ^? key tag) `shouldBe` True

shouldContainKVPair
  :: Value
  -> (Text, Value)
  -> IO ()
shouldContainKVPair template (tag, val) = fromJust (template ^? key tag) `shouldBe` val

getValueUnderKey
  :: Text
  -> Value
  -> Value
getValueUnderKey k t = fromJust $ t ^? key k

ref lname = object [("Ref", String lname)]




