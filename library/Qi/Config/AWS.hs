{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Qi.Config.AWS where

import           Control.Lens
import           Control.Monad.State.Class (MonadState)
import           Data.Char                 (isAlphaNum)
import           Data.Default              (Default, def)
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as SHM
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Protolude

import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.CW
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.SQS
import           Qi.Config.Identifier
import           Qi.Config.Types


data Config = Config {
    _namePrefix   :: Text
  , _nextId       :: Int
  , _waitOnLogger :: Bool
  , _s3Config     :: S3Config
  , _apiGwConfig  :: ApiGwConfig
  , _lbdConfig    :: LambdaConfig
  , _ddbConfig    :: DdbConfig
  , _cfConfig     :: CfConfig
  , _cwConfig     :: CwConfig
  , _sqsConfig    :: SqsConfig
}
  deriving (Eq, Show)

instance Default Config where
  def = Config {
      _namePrefix   = "qmuli"
    , _nextId       = 0  -- global autoincrement id state
    , _waitOnLogger = True
    , _s3Config     = def
    , _apiGwConfig  = def
    , _lbdConfig    = def
    , _ddbConfig    = def
    , _cfConfig     = def
    , _cwConfig     = def
    , _sqsConfig    = def
  }

makeLenses ''Config



underscoreNamePrefixWith
  :: Text
  -> Config
  -> Text
underscoreNamePrefixWith = namePrefixWith "_"

dotNamePrefixWith
  :: Text
  -> Config
  -> Text
dotNamePrefixWith = namePrefixWith "."

namePrefixWith
  :: Text
  -> Text
  -> Config
  -> Text
namePrefixWith sep name config =
  T.concat [config ^. namePrefix, sep, name]


makeAlphaNumeric
  :: Text
  -> Text
makeAlphaNumeric = T.filter isAlphaNum

class (Eq rid, Show rid, Hashable rid) => CfResource r rid | rid -> r, r -> rid where

  rNameSuffix
    :: r
    -> Text
  getName
    :: Config
    -> r
    -> Text
  getMap
    :: Config
    -> SHM.HashMap rid r

  getAllWithIds
    :: Config
    -> [(rid, r)]
  getAllWithIds = SHM.toList . getMap

  getAll
    :: Config
    -> [r]
  getAll = SHM.elems . getMap

  getById
    :: (Show rid, Eq rid, Hashable rid)
    => Config
    -> rid
    -> r
  getById config rid =
    fromMaybe
      (panic $ "Could not reference resource with id: " <> show rid)
      $ SHM.lookup rid $ getMap config

  getLogicalName
    :: Config
    -> r
    -> Text
  getLogicalName config r =
    T.concat [makeAlphaNumeric (getName config r), rNameSuffix r]

  getPhysicalName
    :: Config
    -> r
    -> Text
  getPhysicalName config r =
    makeAlphaNumeric (getName config r) `underscoreNamePrefixWith` config

  getLogicalNameFromId
    :: Config
    -> rid
    -> Text
  getLogicalNameFromId config rid =
    getLogicalName config $ getById config rid

instance CfResource CwEventsRule CwEventsRuleId where
  rNameSuffix = const "CwEventsRule"
  getName _ = (^. cerName)
  getMap = (^. cwConfig . ccRules)


instance CfResource Lambda LambdaId where
  rNameSuffix = const "Lambda"
  getName _ = (^. lbdName)
  getMap = (^. lbdConfig . lbdIdToLambda)


instance CfResource CfCustomResource CfCustomResourceId where
  rNameSuffix = const "CfCustomResource"
  getName config = getLogicalNameFromId config . (^. cLbdId)
  getMap = (^. cfConfig . cfcCustomResources)


instance CfResource DdbTable DdbTableId where
  rNameSuffix = const "DynamoDBTable"
  getName _ = (^. dtName)
  getMap = (^. ddbConfig . dcTables)


instance CfResource S3Bucket S3BucketId where
  rNameSuffix = const "S3Bucket"
  getName _ = (^. s3bName)
  getMap = (^. s3Config . s3IdToBucket)
  getPhysicalName config r =
    makeAlphaNumeric (getName config r) `dotNamePrefixWith` config


instance CfResource Api ApiId where
  rNameSuffix = const "Api"
  getName _ = (^. aName)
  getMap = (^. apiGwConfig . acApis)


instance CfResource ApiAuthorizer ApiAuthorizerId where
  rNameSuffix = const "ApiAuthorizer"
  getName _ = (^. aaName)
  getMap = (^. apiGwConfig . acApiAuthorizers)


instance CfResource ApiResource ApiResourceId where
  rNameSuffix = const "ApiResource"
  getName _ = (^. arName)
  getMap = (^. apiGwConfig . acApiResources)

instance CfResource SqsQueue SqsQueueId where
  rNameSuffix = const "SqsQueue"
  getName _ = (^. sqsQueueName)
  getMap = (^. sqsConfig . sqsQueues)
  getPhysicalName config r =
    makeAlphaNumeric (getName config r) `dotNamePrefixWith` config

