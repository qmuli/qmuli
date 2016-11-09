{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.Api (toResources) where

import           Data.Aeson                     (Value (Array), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Stratosphere                   hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.Api
import           Qi.Config.AWS.Api.Accessors
import           Qi.Config.AWS.Lambda.Accessors
import           Qi.Config.Identifier


toResources config = Resources $ foldMap toStagedApiResources $ getAllApis config

  where
    toStagedApiResources :: (ApiId, Api) -> [Resource]
    toStagedApiResources (aid, api) = deploymentResource:apiResources
      where

        apiResources = [ apiResource ] ++ foldMap toApiChildResources (getApiChildren (Left aid) config)


        apiResName = getApiCFResourceName api

        apiResource = (
          resource apiResName $
            ApiGatewayRestApiProperties $
            apiGatewayRestApi $
              Literal (api ^. aName)
          )

        deploymentResource :: Resource
        deploymentResource = (
            resource name $
              ApiGatewayDeploymentProperties $
              apiGatewayDeployment
                (Ref apiResName)
                & agdStageName ?~ "v1"
          )
          & dependsOn ?~ deps

          where
            name = getApiStageCFResourceName api
            deps = map (\d -> d ^. resName) apiResources


        toApiChildResources :: ApiResourceId -> [Resource]
        toApiChildResources arid =
          [ apirResource ] ++ methodResources ++ foldMap toApiChildResources (getApiChildren (Right arid) config)
          where
            apir = getApiResourceById arid config
            apirResName = getApiResourceCFResourceName apir

            apirResource =
              case apir of
                ApiResource{_arParent = Left aid} -> (
                    resource apirResName $
                      ApiGatewayResourceProperties $
                      apiGatewayResource
                        apiParent
                        (Literal $ apir ^. arName)
                        (Ref apiResName)
                  )


                -- TODO
                ApiResource{_arParent = Right arid'} -> undefined


              where
                apiParent = GetAtt apiResName "RootResourceId"


            methodResources = map toMethodResource (apir ^. arMethodConfigs)

            toMethodResource ApiMethodConfig{_verb, _lbdId} = (
                resource name $
                  ApiGatewayMethodProperties $
                  apiGatewayMethod
                    "NONE"
                    verb
                    (Ref apirResName)
                    (Ref apiResName)
                    & agmeIntegration ?~ integration
                    & agmeMethodResponses ?~ [ methodResponse ]
              )
              & dependsOn ?~ [
                    lbdPermResName
                  ]

              where
                name = getApiMethodCFResourceName apir _verb
                verb = Literal . T.pack $ show _verb
                methodResponse = apiGatewayMethodResponse "200"
                lbdResName = getLambdaResourceNameFromId _lbdId config
                lbdPermResName = getLambdaPermissionResourceName $ getLambdaById _lbdId config

                integration =
                  apiGatewayIntegration "AWS"
                  & agiIntegrationHttpMethod ?~ "POST" -- looks like this should always be "POST"
                  & agiUri ?~ uri
                  & agiPassthroughBehavior ?~ passthroughBehavior
                  & agiRequestTemplates ?~ requestTemplates
                  & agiIntegrationResponses ?~ [ integrationResponse ]

                  where
                    uri = (Join "" [
                        "arn:aws:apigateway:"
                      , Ref "AWS::Region"
                      , ":lambda:path/2015-03-31/functions/"
                      , GetAtt lbdResName "Arn"
                      , "/invocations"])

                    requestTemplates = case _verb of
                      Get   -> []
                      Post  -> [ ("application/json", "{\"body\": $input.body}") ]
                      -- {"body" : $input.json('$')}

                    passthroughBehavior = case _verb of
                      Get  -> "WHEN_NO_TEMPLATES"
                      Post -> "WHEN_NO_TEMPLATES"

                integrationResponse = apiGatewayIntegrationResponse
                  & agirResponseTemplates ?~ responseTemplates
                  & agirStatusCode ?~ "200"

                  where
                    responseTemplates = [ ("application/json", "$input.json('$.body')") ]




