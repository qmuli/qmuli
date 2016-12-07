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

import           Qi.Config.AWS.Api           (ApiEvent)
import           Qi.Config.AWS.CF            (CfEvent)
import           Qi.Config.AWS.S3            (S3Event)
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (LambdaProgram)

data Lambda = S3BucketLambda {
    _lbdName                  :: Text
  , _lbdS3BucketLambdaProgram :: S3Event -> LambdaProgram ()
  }
  | ApiLambda {
    _lbdName                   :: Text
  , _lbdApiMethodLambdaProgram :: ApiEvent -> LambdaProgram ()
  }
  | CfCustomLambda {
    _lbdName                  :: Text
  , _lbdCFCustomLambdaProgram :: CfEvent -> LambdaProgram ()
  }
  deriving Show

makeLenses ''Lambda

instance Hashable Lambda where
  hashWithSalt s lbd = s `hashWithSalt` (lbd ^. lbdName)


data LambdaConfig = LambdaConfig {
    _lcLambdas :: HashMap LambdaId Lambda
  } deriving Show

makeLenses ''LambdaConfig

instance Monoid LambdaConfig where
  LambdaConfig { _lcLambdas = l1 } `mappend` LambdaConfig { _lcLambdas = l2 } =
    LambdaConfig { _lcLambdas = l1 `mappend` l2 }
  mempty = def

instance Default LambdaConfig where
  def = LambdaConfig {
    _lcLambdas = SHM.empty
  }




