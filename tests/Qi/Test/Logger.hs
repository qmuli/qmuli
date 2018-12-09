module Qi.Test.Logger where

import qualified Data.ByteString.Lazy.Builder as Build
import           Protolude
import           Qi.AWS.Types                 (MkAwsLogger)
import           System.IO                    hiding (hPutStrLn)


mkTestLogger
  :: MkAwsLogger
mkTestLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \_lvl b ->
    hPutStrLn stderr $ Build.toLazyByteString b

