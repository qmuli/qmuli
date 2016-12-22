{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Amazonka (runAmazonka, currentRegion) where

import           Control.Lens
import           Control.Monad.Trans.AWS (runAWST, send)
import           Data.Maybe              (fromMaybe)
import           Data.Yaml               (FromJSON (..), Value (Object), decode,
                                          (.:))
import           Network.AWS             hiding (send)
import           Safe                    (readMay)
import           System.IO               (stdout)
import           Text.Heredoc


qmuliConfig = [there|./qmuli.yaml|]

newtype CurrentRegion = CurrentRegion {unCurrentRegion :: Region}
instance FromJSON CurrentRegion where
  parseJSON (Object v) =
    CurrentRegion <$> (v .: "aws" >>= (.: "region"))

currentRegion :: Region
currentRegion = fromMaybe
  (error "could not parse qmuli.yaml")
  $ decode qmuliConfig


runAmazonka
  :: AWS a
  -> IO a
runAmazonka action = do
  {- logger <- newLogger Debug stdout -}
  {- env <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion -}
  env <- newEnv Discover <&> set envRegion currentRegion
  runResourceT $ runAWST env action

