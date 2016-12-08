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
    _rpPath    :: HashMap Text Value
  , _rpHeaders :: HashMap Text Text
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


type ApiAuthorizerDeps = HashMap ApiId [ApiAuthorizerId]
type ApiResourceDeps = HashMap (Either ApiId ApiResourceId) [ApiResourceId]

data ApiConfig = ApiConfig {
    _acApis              :: HashMap ApiId Api
  , _acApiResources      :: HashMap ApiResourceId ApiResource
  , _acApiResourceDeps   :: ApiResourceDeps
  , _acApiAuthorizers    :: HashMap ApiAuthorizerId ApiAuthorizer
  , _acApiAuthorizerDeps :: ApiAuthorizerDeps
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


data ApiVerb
  = Post
  | Get
  | Put
  | Delete
  | Head
  | Options
  deriving Show

data ApiMethodConfig = ApiMethodConfig {
    amcVerb   :: ApiVerb
  , amcAuthId :: Maybe ApiAuthorizerId
  , amcLbdId  :: LambdaId
  } deriving Show

data ApiAuthorizer = ApiAuthorizer {
    _aaName      :: Text
  , _aaCognitoId :: CustomId
  , _aaApiId     :: ApiId
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


instance Hashable ApiAuthorizer where
  hashWithSalt s ApiAuthorizer{_aaName} = s `hashWithSalt` (T.concat [_aaName, "ApiAuthorizer"])

-- TODO: hash not only name of the resource but the full hierarchy path from the root Api
instance Hashable ApiResource where
  hashWithSalt s ApiResource{_arName} = s `hashWithSalt` (T.concat [_arName, "ApiResource"])


instance Default ApiConfig where
  def = ApiConfig {
    _acApis               = SHM.empty
  , _acApiAuthorizers     = SHM.empty
  , _acApiResources       = SHM.empty
  , _acApiAuthorizerDeps  = SHM.empty
  , _acApiResourceDeps    = SHM.empty
  }

makeLenses ''ApiConfig
makeLenses ''Api
makeLenses ''ApiAuthorizer
makeLenses ''ApiResource
makeLenses ''ApiEvent
makeLenses ''RequestParams

