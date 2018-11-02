{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Program.CF.Ipret.Gen (run) where

import           Control.Lens
import           Control.Monad.Freer        hiding (run)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Network.AWS.CloudFormation (Capability (CapabilityNamedIAM), StackStatus (SSCreateComplete, SSDeleteComplete, SSUpdateComplete),
                                             StackStatus, cloudFormation,
                                             createStack, csCapabilities,
                                             csTemplateBody, dStackName,
                                             deleteStack, describeStacks,
                                             dsRetainResources, dsrsStacks,
                                             lsrsStackSummaries, oOutputKey,
                                             oOutputValue, sOutputs, sStackName,
                                             sStackStatus, ssStackName,
                                             ssStackStatus, updateStack,
                                             usCapabilities, usTemplateBody)
import           Network.AWS.S3             (BucketName (BucketName),
                                             ObjectKey (ObjectKey))
import           Protolude                  hiding ((<&>))
import           Qi.Config.AWS
import           Qi.Config.AWS.CF
import           Qi.Config.AWS.S3
import           Qi.Program.CF.Lang         (AbsentDirective (..), CfEff (..),
                                             StackDescription (..),
                                             StackDescriptionDict,
                                             StackName (StackName))
import           Qi.Program.Config.Lang     (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang


run
  :: forall effs a
  .  (Member GenEff effs, Member ConfigEff effs)
  => (Eff (CfEff ': effs) a -> Eff effs a)
run = interpret (\case

  CreateStack (StackName name) template -> do
    config  <- getConfig

    void . amazonka cloudFormation $ createStack name
                & csTemplateBody ?~ toS template
                & csCapabilities .~ [ CapabilityNamedIAM ]


  UpdateStack (StackName name) template -> do
    config  <- getConfig

    void . amazonka cloudFormation $ updateStack name
                & usTemplateBody ?~ toS template
                & usCapabilities .~ [ CapabilityNamedIAM ]


  DeleteStack (StackName name) ->
    void . amazonka cloudFormation $ deleteStack name
                & dsRetainResources .~ []

  DescribeStacks ->
    getStackDescriptions



  WaitOnStackStatus name status' isAbsentOk -> do
    let loop = sleep 1000000 >> go
        go = do
          stackDict <- getStackDescriptions
          case Map.lookup name stackDict of
            Just StackDescription{ status } | status == status' -> pure ()
            Just _  -> loop -- wait for the stack state to change
            Nothing -> case isAbsentOk of -- no mention of the stack in the log
                          AbsentOk -> pure () -- it's fine, don't wait any longer
                          NoAbsent -> loop -- keep waiting for the stack to appear in the log

    go

  )


  where

    getStackDescriptions :: Eff effs StackDescriptionDict
    getStackDescriptions = do
      r <- amazonka cloudFormation $ describeStacks
                  -- & dStackName ?~ name
      pure . Map.fromList $ (\stack ->
        ( StackName $ stack ^. sStackName
        , StackDescription {
              status  = stack ^. sStackStatus
            , outputs = catMaybes $ (\o -> do
                            key <- o ^. oOutputKey
                            val <- o ^. oOutputValue
                            pure (key, val)
                          ) <$> stack ^. sOutputs
            }
        )) <$> r ^. dsrsStacks

{-


updateStack
  :: Text
  -> AWS ()
updateStack name =
  void . send $ CF.updateStack name
            & usTemplateURL ?~ T.concat ["https://s3.amazonaws.com/", name, "/cf.json"]
            & usCapabilities .~ [CapabilityNamedIAM]


deleteStack
  :: Text
  -> AWS ()
deleteStack name =
  void . send $ CF.deleteStack name
                  & dsRetainResources .~ []


describeStack
 :: Text
 -> AWS StackDescription
describeStack name = do
  r <- send $ CF.describeStacks
                & dStackName ?~ name
  case listToMaybe $ r ^ .dsrsStacks of
    Just stack ->
      return $ StackDescription {
          sdStatus = T.pack . show $ stack^.sStackStatus
        , sdOutputs = map (\o -> (fromJust $ o^.oOutputKey, fromJust $ o^.oOutputValue)) $ stack^.sOutputs
        }
    Nothing ->
      panic "Error: no stack description was returned"




- -}
