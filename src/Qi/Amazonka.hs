{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Amazonka (runAmazonka, currentRegion) where

import           Control.Lens
import           Control.Monad.Trans.AWS (runAWST, send)
import           Network.AWS             hiding (send)
import           System.IO               (stdout)


currentRegion = NorthVirginia

runAmazonka action = do
  {- logger <- newLogger Debug stdout -}
  {- env <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion -}
  env <- newEnv Discover <&> set envRegion currentRegion
  runResourceT $ runAWST env action

