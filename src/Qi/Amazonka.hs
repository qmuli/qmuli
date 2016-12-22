{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Amazonka (runAmazonka, currentRegion) where

import           Control.Lens
import           Control.Monad.Trans.AWS (runAWST, send)
import           Data.Aeson              (FromJSON (..), Value (Object),
                                          eitherDecode, (.:))
import           Data.Aeson.Types        (typeMismatch, withObject)
import           Data.Either             (either)
import           Network.AWS             hiding (send)
import           Safe                    (readMay)
import           System.IO               (stdout)
import           Text.Heredoc


qmuliConfig = [there|./qmuli.json|]

newtype CurrentRegion = CurrentRegion {unCurrentRegion :: Region}
instance FromJSON CurrentRegion where
  parseJSON (Object o) =
    CurrentRegion <$> (withObject "AWS configuration" (.: "region") =<< o .: "aws")
  parseJSON v          = typeMismatch "qmuli configuration" v


currentRegion :: Region
currentRegion = unCurrentRegion $
  either
    (error . ("could not parse qmuli.yaml: " ++) )
    id
    $ eitherDecode qmuliConfig


runAmazonka
  :: AWS a
  -> IO a
runAmazonka action = do
  {- logger <- newLogger Debug stdout -}
  {- env <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion -}
  env <- newEnv Discover <&> set envRegion currentRegion
  runResourceT $ runAWST env action

