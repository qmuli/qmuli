{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Program.CF.Lang (
    StackName(StackName)
  , CfEff (..)
  , createStack
  , describeStacks
  , updateStack
  , deleteStack
  , waitOnStackStatus
  , StackStatus(..)
  , AbsentDirective(..)
  , StackDescription(..)
  , StackDescriptionDict
  )where


import           Control.Monad.Freer
import           Data.Aeson                 hiding ((.:))
import qualified Data.ByteString.Lazy       as LBS
import           Data.HashMap.Strict        (fromList)
import           Data.Map                   (Map)
import           Network.AWS.CloudFormation (StackStatus (..))
import           Network.AWS.S3.Types       (ETag)
import           Protolude
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Core.Curry
import           Qi.Program.Gen.Lang


type StackDescriptionDict = Map StackName StackDescription

newtype StackName = StackName Text
  deriving (Eq, Show, Ord, ToJSON)

data StackDescription = StackDescription {
    status  :: StackStatus
  , outputs :: [(Text, Text)]
  } deriving (Show)

instance ToJSON StackDescription where
  toJSON StackDescription{ status, outputs } =
    object  [ "status"  .= String (show status :: Text)
            , "outputs" .= (Object $ fromList $ second String <$> outputs)
            ]


data AbsentDirective = AbsentOk | NoAbsent
  deriving Eq

data CfEff r where

  CreateStack
    :: StackName
    -> S3Object
    -> CfEff ()

  DescribeStacks
    :: CfEff StackDescriptionDict

  UpdateStack
    :: StackName
    -> S3Object
    -> CfEff ()

  DeleteStack
    :: StackName
    -> CfEff ()

  WaitOnStackStatus
    :: StackName
    -> StackStatus
    -> AbsentDirective
    -> CfEff ()

createStack
  :: (Member CfEff effs)
  => StackName
  -> S3Object
  -> Eff effs ()
createStack = send .: CreateStack

describeStacks
  :: (Member CfEff effs)
  => Eff effs StackDescriptionDict
describeStacks = send DescribeStacks

updateStack
  :: (Member CfEff effs)
  => StackName
  -> S3Object
  -> Eff effs ()
updateStack = send .: UpdateStack

deleteStack
  :: (Member CfEff effs)
  => StackName
  -> Eff effs ()
deleteStack = send . DeleteStack

waitOnStackStatus
  :: (Member CfEff effs)
  => StackName
  -> StackStatus
  -> AbsentDirective
  -> Eff effs ()
waitOnStackStatus = send .:: WaitOnStackStatus

