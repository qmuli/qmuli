
module Main where

import           Protolude
import           Qi.Config.AWS.CF
{- import qualified Qi.Test.CF.CustomResource                as CF.CustomResource -}
{- import qualified Qi.Test.Config.ApiGw.Simple              as ApiGw.Simple -}
{- import qualified Qi.Test.Config.ApiGw.UserPoolsAuthorizer as ApiGw.UserPoolsAuthorizer -}
{- import qualified Qi.Test.Config.DDB.Simple                as DDB.Simple -}
{- import qualified Qi.Test.Config.DDB.Stream                as DDB.Stream -}
{- import qualified Qi.Test.Config.Identifier                as Id -}
{- import qualified Qi.Test.Config.Template                  as Template -}
import qualified Qi.Test.Config.Eff as ConfigEff
import           Test.Tasty
import           Test.Tasty.Hspec


main :: IO ()
main = do
  tree <- sequence specs
  defaultMain $ testGroup "Qmuli" tree

  where
    specs = [
        testSpec "ConfigEff" ConfigEff.spec
        {- testSpec "Template"                   Template.spec -}
      {- , testSpec "Identifier"                 Id.spec -}
      {- , testSpec "ApiGw Simple"               ApiGw.Simple.spec -}
      {- , testSpec "ApiGw UserPools Authorizer" ApiGw.UserPoolsAuthorizer.spec -}
      {- , testSpec "DDB Simple"                 DDB.Simple.spec -}
      {- , testSpec "DDB Stream"                 DDB.Stream.spec -}
      {- , testSpec "Cloud Formation"            CF.CustomResource.spec -}
      ]

