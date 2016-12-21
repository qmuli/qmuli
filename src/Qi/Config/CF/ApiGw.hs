{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Config.CF.ApiGw (toResources, toOutputs) where

import           Data.Aeson                                  (Value (Bool),
                                                              object)
import qualified Data.ByteString.Lazy                        as LBS
import qualified Data.HashMap.Strict                         as SHM
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import           Qi.Config.AWS.ApiGw.Api.Accessors
import           Qi.Config.AWS.ApiGw.ApiAuthorizer.Accessors
import           Qi.Config.AWS.ApiGw.ApiDeployment.Accessors
import           Qi.Config.AWS.ApiGw.ApiMethod.Accessors
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile       (ampAuthId)
import           Qi.Config.AWS.ApiGw.ApiResource.Accessors
import           Qi.Config.AWS.CF.Accessors
import           Qi.Config.AWS.Lambda.Accessors
import           Qi.Config.Identifier
import           Stratosphere                                hiding (Delete,
                                                              name)
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

        apiLName = getApiLogicalName api

        apiResource = (
          resource apiLName $
            ApiGatewayRestApiProperties $
            apiGatewayRestApi
            & agraName ?~ Literal (api ^. aName)
          )

        deploymentResource :: Resource
        deploymentResource = (
            resource name $
              ApiGatewayDeploymentProperties $
              apiGatewayDeployment
                (Ref apiLName)
                & agdStageName ?~ "v1"
          )
          & dependsOn ?~ deps

          where
            name = getApiStageLogicalName api
            deps = map (^. resName) apiResources


        toApiAuthorizers :: ApiAuthorizerId -> Resource
        toApiAuthorizers aaid = (
          resource name $
            ApiGatewayAuthorizerProperties $
            apiGatewayAuthorizer
              & agaProviderARNs ?~ [userPoolArn]
              & agaRestApiId ?~ (Ref apiLName)
              & agaName ?~ Literal (auth^.aaName)
              & agaType ?~ Literal COGNITO_USER_POOLS_AUTH
              & agaIdentitySource ?~ "method.request.header.Authorization"
          )
          {- & dependsOn ?~ [cognitoLName] -}

          where
            userPoolArn = Join "" [
                "arn:aws:cognito-idp:"
              , Ref "AWS::Region"
              , ":"
              , Ref "AWS::AccountId"
              , ":userpool/"
              , userPoolPhysicalId
              ]
            userPoolPhysicalId = GetAtt cognitoLName "UserPoolId"

            name = getApiAuthorizerLogicalName $ getApiAuthorizerById aaid config
            auth = getApiAuthorizerById aaid config
            cognito = getCustomById (auth^.aaCognitoId) config
            cognitoLName = getCustomLogicalName cognito config



        toApiChildResources :: ApiResourceId -> [Resource]
        toApiChildResources arid =
          [ apirResource ] ++
          methodResources ++
          foldMap toApiChildResources (getApiChildren (Right arid) config)

          where
            apir = getApiResourceById arid config
            apirLName = getApiResourceLogicalName apir

            apirResource =
              case apir of
                ApiResource{_arParent = Left aid} ->
                  let
                    apiParent = GetAtt apiLName "RootResourceId"
                  in
                    resource apirLName $
                      ApiGatewayResourceProperties $
                      apiGatewayResource
                        apiParent
                        (Literal $ apir ^. arName)
                        (Ref apiLName)


                ApiResource{_arParent = Right arid'} ->
                  let
                    apirParentLName = getApiResourceLogicalName $ getApiResourceById arid' config
                  in
                    resource apirLName $
                      ApiGatewayResourceProperties $
                      apiGatewayResource
                        (Ref apirParentLName)
                        (Literal $ apir ^. arName)
                        (Ref apiLName)



            methodResources = [ corsMethodResource ] ++ map toMethodResource (apir ^. arMethodConfigs)

            corsMethodResource =
                resource name $
                  ApiGatewayMethodProperties $
                  apiGatewayMethod
                    (Literal OPTIONS)
                    & agmeAuthorizationType ?~ Literal NONE
                    & agmeResourceId ?~ (Ref apirLName)
                    & agmeRestApiId ?~ (Ref apiLName)
                    & agmeIntegration ?~ integration
                    & agmeMethodResponses ?~ [ methodResponse ]

              where
                name = getApiMethodLogicalName apir Options

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


            toMethodResource ApiMethodConfig{amcVerb, amcLbdId, amcProfile} = (
                resource name $
                  ApiGatewayMethodProperties $
                  specAuth $
                  apiGatewayMethod
                    (Literal $ verb amcVerb)
                    & agmeResourceId ?~ (Ref apirLName)
                    & agmeRestApiId ?~ (Ref apiLName)
                    & agmeIntegration ?~ integration
                    & agmeMethodResponses ?~ methodResponses

              )
              & dependsOn ?~ [
                    lbdLName
                  , lbdPermLName
                  ]

              where
                specAuth res = case amcProfile^.ampAuthId of
                  Nothing ->
                    res
                    & agmeAuthorizationType ?~ Literal NONE

                  Just authId ->
                    let
                      authLName = getApiAuthorizerLogicalName $ getApiAuthorizerById authId config
                    in
                    res
                    & agmeAuthorizationType ?~ Literal COGNITO_USER_POOLS
                    & agmeAuthorizerId ?~ Ref authLName

                name = getApiMethodLogicalName apir amcVerb

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

                lbdId = getLambdaById amcLbdId config
                lbdLName = getLambdaLogicalName lbdId
                lbdPermLName = getLambdaPermissionLogicalName lbdId

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
                      , GetAtt lbdLName "Arn"
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

toOutputs
  :: Config
  -> Outputs
toOutputs config =
  Outputs . map toApiOutput $ getAllApis config

  where

    toApiOutput (_, api) =
      output (T.concat [apiLName, "URL"])
        apiUrl
        & description ?~ "RestApi URL"

      where
        -- https://{restapi_id}.execute-api.{region}.amazonaws.com/{stage_name}/
        apiUrl = Join "" [
            "https://"
          , Ref apiLName
          , ".execute-api.us-east-1.amazonaws.com/v1"
          ]

        apiLName = getApiLogicalName api


