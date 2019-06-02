{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Test.Util
  ( authHeader
  , contentTypeJson
  , defaultApp
  , matchJSON
  , jsonRequest
  , emptyBody
  ) where

import           Control.Exception (catch, throw)
import           Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
import           FileCollector.Backend.Main (mainAsWai)
import           FileCollector.Backend.TestData.Simple (populateTestData)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai (Application)
import           Network.Wai.Test (SResponse)
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (WriteMode), withFile)
import           System.IO.Error (isDoesNotExistError)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
    (MatchBody (..), ResponseMatcher (..), (<:>))

import FileCollector.Backend.Config
import FileCollector.Common.Types
import Paths_file_collector_backend (getDataDir)

authHeader :: UserName -> Password -> Header
authHeader (UserName username) (Password password) = ("Authorization", value)
  where
    value = "Basic " <> Base64.encode (T.encodeUtf8 $ username <> ":" <> password)

contentTypeJson :: Header
contentTypeJson = ("Content-Type", "application/json;charset=utf-8")

defaultApp :: IO Application
defaultApp = do
    dataDir <- getDataDir
    config :: Maybe OtherConfig <-
      Aeson.decodeFileStrict' (dataDir </> "config" </> "int_test.json")
    case config of
      Nothing -> error "Failed to read inttest config"
      Just config' -> do
        let databaseDir = dataDir </> "devres"
            connStr = dataDir </> "devres" </> "int_test.db"
        createDirectoryIfMissing True databaseDir
        withFile connStr WriteMode (const $ pure ())
        catch (removeFile connStr) $ \e ->
          if isDoesNotExistError e then pure () else throw e
        mainAsWai (set otherConfig_dbConnStr (T.pack connStr) config') $
          Db.withConnection populateTestData

matchJSON :: Aeson.ToJSON a => a -> ResponseMatcher
matchJSON value =
    ResponseMatcher 200
      [ "Content-Type" <:> "application/json;charset=utf-8" ]
      matchJsonBody
  where
    matchJsonBody = MatchBody $ \_ body ->
      case Aeson.decode body of
        Just bodyValue ->
          if bodyValue == value'
          then Nothing
          else Just "json value mismatch"
        Nothing -> Just "Unable to parse response body as json"
    value' = Aeson.toJSON value

jsonRequest :: Aeson.ToJSON body
            => Method -- ^method name
            -> Text -- ^URL
            -> [Header]
            -> body
            -> WaiSession SResponse
jsonRequest method url headers reqBody =
  request
    method
    (T.encodeUtf8 url)
    (contentTypeJson : headers)
    (Aeson.encode reqBody)

emptyBody :: Text
emptyBody = ""
