{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM, void)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Default                    (def)
import           Data.Hashable                   (Hashable, hash)
import qualified Data.HashMap.Strict             as SHM
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Scan
import           Web.JWT                         (claims, decode)

import           Qi                              (withConfig)
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..))
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface     (ConfigProgram, customResource,
                                                  ddbTable, genericLambda)
import           Qi.Program.Lambda.Interface     (CompleteLambdaProgram,
                                                  GenericLambdaProgram,
                                                  deleteDdbRecord, getDdbRecord,
                                                  putDdbRecord, say,
                                                  scanDdbRecords)
import           Qi.Util
import           Qi.Util.Cognito                 (cognitoPoolProviderLambda)
import           Qi.Util.DDB

import           Types
import           Types.Contact


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      cognito <- customResource "cognitoPoolProvider" cognitoPoolProviderLambda $ def

      contactsTable <- ddbTable "contacts" (DdbAttrDef "Id" S) def

      genericLambda "scanContacts"  (scanContacts contactsTable) def
      genericLambda "postContact"   (postContact contactsTable) def
      genericLambda "getContact"    (getContact contactsTable) def
      genericLambda "putContact"    (putContact contactsTable) def
      genericLambda "deleteContact" (deleteContact contactsTable) def

      {- genericLambda "getContactLogs" (getContactLogs contactsTable) def -}
      {- genericLambda "putContactLog"  (putContactLog contactsTable) def -}

      return ()



scanContacts
  :: DdbTableId
  -> GenericLambdaProgram
scanContacts ddbTableId payload = do
  r <- scanDdbRecords ddbTableId
  withSuccess (r^.srsResponseStatus) $
    result
      (internalError . ("Parsing error: " ++))
      (success . (toJSON :: [Contact] -> Value))
      $ forM (r^.srsItems) parseAttrs

postContact
  :: DdbTableId
  -> GenericLambdaProgram
postContact ddbTableId payload =
  withDeserializedPayload payload $ \(contact :: Contact) -> do
    r <- putDdbRecord ddbTableId . toAttrs $ addId contact
    withSuccess (r^.pirsResponseStatus) $
      success "successfully posted contact"
  where
    addId :: Contact -> Contact
    addId c = c{cId = T.pack . show $ hash c}

getContact
  :: DdbTableId
  -> GenericLambdaProgram
getContact ddbTableId payload =
  withId payload $ \cid -> do
    r <- getDdbRecord ddbTableId $ idKeys cid
    withSuccess (r^.girsResponseStatus) $
      result
        (internalError . ("Parsing error: " ++))
        (success . (toJSON :: Contact -> Value))
        $ parseAttrs $ r^.girsItem


putContact
  :: DdbTableId
  -> GenericLambdaProgram
putContact ddbTableId payload =
  withDeserializedPayload payload $ \(contact :: Contact) -> do
    r <- putDdbRecord ddbTableId $ toAttrs contact
    withSuccess (r^.pirsResponseStatus) $
      success "successfully put contact"

deleteContact
  :: DdbTableId
  -> GenericLambdaProgram
deleteContact ddbTableId payload =
  withId payload $ \cid -> do
      r <- deleteDdbRecord ddbTableId $ idKeys cid
      withSuccess (r^.dirsResponseStatus) $
        success "successfully deleted contact"


withDeserializedPayload
  :: FromJSON a
  => Value
  -> (a -> CompleteLambdaProgram)
  -> CompleteLambdaProgram
withDeserializedPayload payload f =
  result
    (internalError . ("Error: fromJson: " ++))
    f
    $ fromJSON payload

withId
  :: Value
  -> (Text -> CompleteLambdaProgram)
  -> CompleteLambdaProgram
withId payload f =
  maybe
    (argumentsError "payload is missing the 'Id' attribute")
    f
    $ payload ^? key "Id" . _String

