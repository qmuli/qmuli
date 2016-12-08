{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Config.CF.Api (toResources, toOutputs) where

import           Data.Aeson                     (Value (Bool), object)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as SHM
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Qi.Config.AWS
import           Qi.Config.AWS.Api
import           Qi.Config.AWS.Api.Accessors
import           Qi.Config.AWS.CF.Accessors
import           Qi.Config.AWS.Lambda.Accessors
import           Qi.Config.Identifier
import           Stratosphere                   hiding (Delete, name)
import           Text.Heredoc


toResources config = Resources . foldMap toStagedApiResources $ getAllApis config

  where
    jsonContentType = "application/json"

    toStagedApiResources :: (ApiId, Api) -> [Resource]
    toStagedApiResources (aid, api) = deploymentResource:apiResources
      where
        apiResources = [ apiResource ]
          ++ map toApiAuthorizers (getApiAuthorizers aid config)
          ++ foldMap toApiChildResources (getApiChildren (Left aid) config)

        apiResName = getApiCFResourceName api

        apiResource = (
          resource apiResName $
            ApiGatewayRestApiProperties $
            apiGatewayRestApi
            & agraName ?~ Literal (api ^. aName)
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
            deps = map (^. resName) apiResources


        toApiAuthorizers :: ApiAuthorizerId -> Resource
        toApiAuthorizers aaid = (
          resource name $
            ApiGatewayAuthorizerProperties $
            apiGatewayAuthorizer
              & agaProviderARNs ?~ [userPoolArn]
              & agaRestApiId ?~ (Ref apiResName)
              & agaName ?~ Literal (auth^.aaName)
              & agaType ?~ "COGNITO_USER_POOLS"
              & agaIdentitySource ?~ "method.request.header.Authorization"
          )
          {- & dependsOn ?~ [cognitoResName] -}

          where
            userPoolArn = Join "" [
                "arn:aws:cognito-idp:"
              , Ref "AWS::Region"
              , ":"
              , Ref "AWS::AccountId"
              , ":userpool/"
              , userPoolPhysicalId
              ]
            userPoolPhysicalId = GetAtt cognitoResName "UserPoolId"

            name = getApiAuthorizerCFResourceName $ getApiAuthorizerById aaid config
            auth = getApiAuthorizerById aaid config
            cognito = getCustomById (auth^.aaCognitoId) config
            cognitoResName = getCustomCFResourceName cognito config



        toApiChildResources :: ApiResourceId -> [Resource]
        toApiChildResources arid =
          [ apirResource ] ++
          methodResources ++
          foldMap toApiChildResources (getApiChildren (Right arid) config)

          where
            apir = getApiResourceById arid config
            apirResName = getApiResourceCFResourceName apir

            apirResource =
              case apir of
                ApiResource{_arParent = Left aid} ->
                  let
                    apiParent = GetAtt apiResName "RootResourceId"
                  in
                    resource apirResName $
                      ApiGatewayResourceProperties $
                      apiGatewayResource
                        apiParent
                        (Literal $ apir ^. arName)
                        (Ref apiResName)


                ApiResource{_arParent = Right arid'} ->
                  let
                    apirParentResName = getApiResourceCFResourceName $ getApiResourceById arid' config
                  in
                    resource apirResName $
                      ApiGatewayResourceProperties $
                      apiGatewayResource
                        (Ref apirParentResName)
                        (Literal $ apir ^. arName)
                        (Ref apiResName)



            methodResources = [ corsMethodResource ] ++ map toMethodResource (apir ^. arMethodConfigs)

            corsMethodResource =
                resource name $
                  ApiGatewayMethodProperties $
                  apiGatewayMethod
                    (Literal OPTIONS)
                    & agmeAuthorizationType ?~ Literal NONE
                    & agmeResourceId ?~ (Ref apirResName)
                    & agmeRestApiId ?~ (Ref apiResName)
                    & agmeIntegration ?~ integration
                    & agmeMethodResponses ?~ [ methodResponse ]

              where
                name = getApiMethodCFResourceName apir Options

                methodResponse = apiGatewayMethodMethodResponse
                  & agmmrResponseModels ?~ responseModels
                  & agmmrResponseParameters ?~ responseParams
                  & agmmrStatusCode ?~ "200"

                  where
                    responseModels = [(jsonContentType, "Empty")]

                    responseParams = [
                        ("method.response.header.Access-Control-Allow-Headers", Bool False)
                      , ("method.response.header.Access-Control-Allow-Methods", Bool False)
                      , ("method.response.header.Access-Control-Allow-Origin", Bool False)
                      ]


                integration =
                  apiGatewayMethodIntegration
                  & agmiType ?~ Literal MOCK
                  & agmiIntegrationHttpMethod ?~ Literal POST -- looks like this should always be "POST"
                  & agmiPassthroughBehavior ?~ Literal WHEN_NO_MATCH
                  & agmiRequestTemplates ?~ requestTemplates
                  & agmiIntegrationResponses ?~ [ integrationResponse ]

                  where
                    requestTemplates = [ (jsonContentType, "{\"statusCode\": 200}") ]

                    integrationResponse = apiGatewayMethodIntegrationResponse
                      & agmirResponseTemplates ?~ responseTemplates
                      & agmirResponseParameters ?~ responseParams
                      & agmirStatusCode ?~ "200"

                      where
                        responseTemplates = [ (jsonContentType, "") ]
                        responseParams = [
                            ("method.response.header.Access-Control-Allow-Headers", "'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'")
                          , ("method.response.header.Access-Control-Allow-Methods", "'DELETE,GET,HEAD,POST,PUT,OPTIONS,TRACE'")
                          , ("method.response.header.Access-Control-Allow-Origin", "'*'")
                          ]


            toMethodResource ApiMethodConfig{amcVerb, amcLbdId, amcAuthId} = (
                resource name $
                  ApiGatewayMethodProperties $
                  specAuth $
                  apiGatewayMethod
                    (Literal $ verb amcVerb)
                    & agmeResourceId ?~ (Ref apirResName)
                    & agmeRestApiId ?~ (Ref apiResName)
                    & agmeIntegration ?~ integration
                    & agmeMethodResponses ?~ methodResponses

              )
              & dependsOn ?~ [
                    lbdPermResName
                  ]

              where
                specAuth res = case amcAuthId of
                  Nothing ->
                    res
                    & agmeAuthorizationType ?~ Literal NONE

                  Just authId ->
                    let
                      authResName = getApiAuthorizerCFResourceName $ getApiAuthorizerById authId config
                    in
                    res
                    & agmeAuthorizationType ?~ Literal COGNITO_USER_POOLS
                    & agmeAuthorizerId ?~ Ref authResName

                name = getApiMethodCFResourceName apir amcVerb

                verb Get     = GET
                verb Post    = POST
                verb Put     = PUT
                verb Delete  = DELETE
                verb Head    = HEAD
                verb Options = OPTIONS

                -- these are all possible response types (statuses) for this method
                methodResponses = map methodResponse ["200", "400", "404", "500"]
                  where

                    methodResponse status = apiGatewayMethodMethodResponse
                      & agmmrResponseParameters ?~ responseParams
                      & agmmrStatusCode ?~ status


                    responseParams = [
                        ("method.response.header.Access-Control-Allow-Headers", Bool False)
                      , ("method.response.header.Access-Control-Allow-Methods", Bool False)
                      , ("method.response.header.Access-Control-Allow-Origin", Bool False)
                      ]

                lbdResName = getLambdaCFResourceNameFromId amcLbdId config
                lbdPermResName = getLambdaPermissionCFResourceName $ getLambdaById amcLbdId config

                integration =
                  apiGatewayMethodIntegration
                  & agmiType ?~ Literal AWS
                  & agmiIntegrationHttpMethod ?~ Literal POST -- looks like this should always be "POST" no matter what the http verb was used on the endpoint
                  & agmiUri ?~ uri
                  & agmiPassthroughBehavior ?~ passthroughBehavior
                  & agmiRequestTemplates ?~ requestTemplates
                  & agmiIntegrationResponses ?~ integrationResponses

                  where
                    uri = (Join "" [
                        "arn:aws:apigateway:"
                      , Ref "AWS::Region"
                      , ":lambda:path/2015-03-31/functions/"
                      , GetAtt lbdResName "Arn"
                      , "/invocations"])

                    requestTemplates =
                      -- TODO: all the same for now. Need to figure out how it should differ for different verbs
                      [ (jsonContentType, postTemplate) ]
                      {- case amcVerb of -}
                        {- Get  -> [ (jsonContentType, postTemplate) ] -}
                        {- Post -> [ (jsonContentType, postTemplate) ] -}

                      where
                        {- getTemplate   = [there|./js/get_template.js|] -}
                        postTemplate  = [there|./js/post_template.js|]

                    passthroughBehavior =
                      -- TODO: all the same for now. Need to figure out how it should differ for different verbs
                      Literal WHEN_NO_TEMPLATES
                      {- case amcVerb of -}
                        {- Get  -> WHEN_NO_TEMPLATES -}
                        {- Post -> WHEN_NO_TEMPLATES -}


                    integrationResponses = [ successIntegrationResponse ] ++
                      map errorIntegrationResponse [
                          "400"
                        , "404"
                        , "500"
                        ]

                    successIntegrationResponse = apiGatewayMethodIntegrationResponse
                      & agmirResponseTemplates ?~ responseTemplates
                      & agmirStatusCode ?~ "200"
                      & agmirResponseParameters ?~ responseParams

                      where
                        responseTemplates = [ (jsonContentType, [there|./js/success_response_template.js|]) ]

                    errorIntegrationResponse errorStatus = apiGatewayMethodIntegrationResponse
                      & agmirResponseTemplates ?~ responseTemplates
                      & agmirStatusCode ?~ (Literal $ T.pack errorStatus)
                      & agmirSelectionPattern ?~ (Literal . T.pack $ "^\\[" ++ errorStatus ++ "\\].*")
                      & agmirResponseParameters ?~ responseParams

                      where
                        responseTemplates = [ (jsonContentType, [there|./js/error_response_template.js|]) ]

                    responseParams = [
                        ("method.response.header.Access-Control-Allow-Headers", "'Content-Type,X-Amz-Date,Authorization,X-Api-Key'")
                      , ("method.response.header.Access-Control-Allow-Methods", "'DELETE,GET,HEAD,POST,PUT,OPTIONS,TRACE'")
                      , ("method.response.header.Access-Control-Allow-Origin", "'*'")
                      ]


toOutputs config =
  Outputs . map toApiOutput $ getAllApis config

  where

    toApiOutput (_, api) =
      output (T.concat [apiResName, "URL"])
        apiUrl
        & description ?~ "RestApi URL"

      where
        -- https://{restapi_id}.execute-api.{region}.amazonaws.com/{stage_name}/
        apiUrl = Join "" [
            "https://"
          , Ref apiResName
          , ".execute-api.us-east-1.amazonaws.com/v1"
          ]

        apiResName = getApiCFResourceName api


