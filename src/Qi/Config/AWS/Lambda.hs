{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.Lambda where

import           Control.Lens
import           Data.Aeson                  (Value)
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Default                (Default, def)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import           Stratosphere

import           Protolude
import           Qi.Config.AWS.ApiGw         (ApiMethodEvent)
import           Qi.Config.AWS.CF            (CfEvent)
import           Qi.Config.AWS.CW            (CwEvent)
import           Qi.Config.AWS.DDB           (DdbStreamEvent)
import           Qi.Config.AWS.S3            (S3Event)
import           Qi.Config.Identifier
import           Qi.Program.Lambda.Interface (CompleteLambdaProgram)


data Lambda =
    GenericLambda {
    _lbdName                 :: Text
  , _lbdProfile              :: LambdaProfile
  , _lbdGenericLambdaProgram :: Value -> CompleteLambdaProgram
  }
  | S3BucketLambda {
    _lbdName                  :: Text
  , _lbdProfile               :: LambdaProfile
  , _lbdS3BucketLambdaProgram :: S3Event -> CompleteLambdaProgram
  }
  | ApiLambda {
    _lbdName                   :: Text
  , _lbdProfile                :: LambdaProfile
  , _lbdApiMethodLambdaProgram :: ApiMethodEvent -> CompleteLambdaProgram
  }
  | CfCustomLambda {
    _lbdName                  :: Text
  , _lbdProfile               :: LambdaProfile
  , _lbdCfCustomLambdaProgram :: CfEvent -> CompleteLambdaProgram
  }
  | CwEventLambda {
    _lbdName                 :: Text
  , _lbdProfile              :: LambdaProfile
  , _lbdCwEventLambdaProgram :: CwEvent -> CompleteLambdaProgram
  }
  | DdbStreamLambda {
    _lbdName                   :: Text
  , _lbdProfile                :: LambdaProfile
  , _lbdDdbStreamLambdaProgram :: DdbStreamEvent -> CompleteLambdaProgram
  }

data LambdaConfig = LambdaConfig {
    _lcLambdas :: HashMap LambdaId Lambda
  }

instance Default LambdaConfig where
  def = LambdaConfig {
    _lcLambdas = SHM.empty
  }


data LambdaMemorySize =
    M128
  | M192
  | M256
  | M320
  | M384
  | M448
  | M512
  | M1024
  | M1536

instance Enum LambdaMemorySize where
  toEnum 128  = M128
  toEnum 192  = M192
  toEnum 256  = M256
  toEnum 320  = M320
  toEnum 384  = M384
  toEnum 448  = M448
  toEnum 512  = M512
  toEnum 1024 = M1024
  toEnum 1536 = M1536

  fromEnum M128  = 128
  fromEnum M192  = 192
  fromEnum M256  = 256
  fromEnum M320  = 320
  fromEnum M384  = 384
  fromEnum M448  = 448
  fromEnum M512  = 512
  fromEnum M1024 = 1024
  fromEnum M1536 = 1536


data LambdaProfile = LambdaProfile {
    _lpMemorySize     :: LambdaMemorySize
  , _lpTimeoutSeconds :: Int
  }

instance Default LambdaProfile where
  def = LambdaProfile {
      _lpMemorySize     = M128
    , _lpTimeoutSeconds = 30
    }


makeLenses ''Lambda
makeLenses ''LambdaConfig
makeLenses ''LambdaProfile

