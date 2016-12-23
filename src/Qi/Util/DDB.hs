{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Util.DDB where

import           Control.Lens         hiding (view, (.=))
import           Data.Aeson
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.AWS.DynamoDB (AttributeValue, attributeValue, avN, avS)


class FromAttrs a where
  parseAttrs
    :: HashMap Text AttributeValue
    -> Result a

class ToAttrs a where
  toAttrs
    :: a
    -> HashMap Text AttributeValue

parseStringAttr
  :: Text
  -> HashMap Text AttributeValue
  -> Result Text
parseStringAttr attrName hm =
  maybe
    (Error $ "could not parse attribute: " ++ T.unpack attrName)
    return
    $ (^.avS) =<< SHM.lookup attrName hm

parseNumberAttr
  :: Text
  -> HashMap Text AttributeValue
  -> Result Int
parseNumberAttr attrName hm =
  maybe
    (Error $ "could not parse attribute: " ++ T.unpack attrName)
    return
    $ (fmap (read . T.unpack) . (^.avN)) =<< SHM.lookup attrName hm


stringAttr
  :: Text
  -> AttributeValue
stringAttr s = attributeValue & avS .~ Just s

numberAttr
  :: Int
  -> AttributeValue
numberAttr n = attributeValue & avN .~ Just (T.pack $ show n)


