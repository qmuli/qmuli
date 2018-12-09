module Main where

import           Protolude
{- import           Qi.Config.AWS.CF -}
import qualified Qi.Test.CF.CustomResource        as CF.CustomResource
{- import qualified Qi.Test.Config.ApiGw.Simple              as ApiGw.Simple -}
{- import qualified Qi.Test.Config.ApiGw.UserPoolsAuthorizer as ApiGw.UserPoolsAuthorizer -}
{- import qualified Qi.Test.Config.DDB.Simple                as DDB.Simple -}
{- import qualified Qi.Test.Config.DDB.Stream                as DDB.Stream -}
import qualified Qi.Test.CLI.Deploy               as Deploy
import qualified Qi.Test.Config.Eff               as ConfigEff
import qualified Qi.Test.Config.Identifier        as Id
import           Qi.Test.Integration.SimpleS3Copy as SimpleS3Copy
{- import           Test.Tasty -}
{- import           Test.Tasty.Hspec -}
import           Test.Hspec


main :: IO ()
main = hspec $ do
  SimpleS3Copy.spec
  ConfigEff.spec
  Id.spec
  CF.CustomResource.spec
  Deploy.spec

{-
  tree <- sequence specs
  defaultMain $ testGroup "Qmuli" tree

  where
    specs = [
        testSpec "ConfigEff"          ConfigEff.spec
      , testSpec "Identifier"         Id.spec
      {- , testSpec "ApiGw Simple"               ApiGw.Simple.spec -}
      {- , testSpec "ApiGw UserPools Authorizer" ApiGw.UserPoolsAuthorizer.spec -}
      {- , testSpec "DDB Simple"                 DDB.Simple.spec -}
      {- , testSpec "DDB Stream"                 DDB.Stream.spec -}
      , testSpec "CF Custom Resource" CF.CustomResource.spec
      , testSpec "Deploy" Deploy.spec

      , testSpec "SimpleS3Copy" SimpleS3Copy.spec
      ]
-}
