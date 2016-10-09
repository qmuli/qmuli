{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.Lambda where

import           Data.Default                (Default, def)
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import           Stratosphere

import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (LambdaProgram)

data Lambda = S3BucketLambda {
    _lbdName                   :: Text
  , _lbdS3BucketLambdaBucketId :: S3BucketIdentifier
  , _lbdS3BucketLambdaProgram  :: S3Event -> LambdaProgram ()
} deriving Show

instance Show (S3Event -> LambdaProgram ()) where
  show _ = "..."

instance Hashable Lambda where
  hashWithSalt s S3BucketLambda{_lbdName} = s `hashWithSalt` _lbdName

makeLenses ''Lambda


data LambdaConfig = LambdaConfig {
  _lcLambdas :: HashMap LambdaIdentifier Lambda
} deriving Show

instance Monoid LambdaConfig where
  LambdaConfig { _lcLambdas = l1 } `mappend` LambdaConfig { _lcLambdas = l2 } =
    LambdaConfig { _lcLambdas = l1 `mappend` l2 }
  mempty = def

instance Default LambdaConfig where
  def = LambdaConfig {
    _lcLambdas = SHM.empty
  }

makeLenses ''LambdaConfig



