{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.Api where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.Identifier


data RequestParams = RequestParams {
    _rpPath :: HashMap Text Value
  } deriving Show

data RequestBody =
    EmptyBody
  | PlainTextBody { unPlainTextBody :: Text }
  | JsonBody { unJsonBody :: Value }
  deriving Show

data ApiEvent = ApiEvent {
    _aeParams :: RequestParams
  , _aeBody   :: RequestBody
  } deriving Show


type ApiDeps = HashMap (Either ApiId ApiResourceId) [ApiResourceId]

data ApiConfig = ApiConfig {
    _acApis         :: HashMap ApiId Api
  , _acApiResources :: HashMap ApiResourceId ApiResource
  , _acApiDeps      :: ApiDeps
  } deriving Show

data Api = Api {
    _aName :: Text
  } deriving Show

instance Default Api where
  def = Api {
    _aName = "default"
  }


instance Hashable Api where
  hashWithSalt s Api{_aName} = s `hashWithSalt` (T.concat [_aName, "Api"])


data ApiVerb = Post | Get | Put | Head | Option

instance Show ApiVerb where
  show Post   = "POST"
  show Get    = "GET"
  show Put    = "PUT"
  show Head   = "HEAD"
  show Option = "OPTION"


data ApiMethodConfig = ApiMethodConfig {
    _verb  :: ApiVerb
  , _lbdId :: LambdaId
  } deriving Show


data ApiResource = ApiResource {
    _arName          :: Text
  , _arParent        :: Either ApiId ApiResourceId
  , _arMethodConfigs :: [ApiMethodConfig]
  } deriving Show

apiResource
  :: Text
  -> Either ApiId ApiResourceId
  -> ApiResource
apiResource name parentId =
  ApiResource {
    _arName           = name
  , _arParent         = parentId
  , _arMethodConfigs  = []
  }


-- TODO: hash not only name of the resource but the full hierarchy path from the root Api
instance Hashable ApiResource where
  hashWithSalt s ApiResource{_arName} = s `hashWithSalt` (T.concat [_arName, "ApiResource"])


instance Monoid ApiConfig where
  ApiConfig {
      _acApis = as1
    , _acApiResources = ars1
    } `mappend`
    ApiConfig {
      _acApis = as2
    , _acApiResources = ars2
    } =
    ApiConfig {
      _acApis = as1 `mappend` as2
    , _acApiResources = ars1 `mappend` ars2
    }
  mempty = def

instance Default ApiConfig where
  def = ApiConfig {
    _acApis         = SHM.empty
  , _acApiResources = SHM.empty
  , _acApiDeps      = SHM.empty
  }

makeLenses ''ApiConfig
makeLenses ''Api
makeLenses ''ApiResource
makeLenses ''ApiEvent
makeLenses ''RequestParams

