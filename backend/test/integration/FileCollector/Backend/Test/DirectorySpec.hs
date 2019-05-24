{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Test.DirectorySpec
  ( spec
  ) where

import           Control.Exception (catch, throw)
import           Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai (Application)
import           System.Directory (removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import qualified Data.ByteString.Base64 as Base64
import           FileCollector.Backend.Config
import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
import           FileCollector.Backend.Main (mainAsWai)
import           FileCollector.Backend.TestData.Simple (populateTestData)
import           FileCollector.Common.Types
import           Paths_file_collector_backend (getDataDir)

app :: IO Application
app = do
    dataDir <- getDataDir
    config :: Maybe OtherConfig <-
      Aeson.decodeFileStrict' (dataDir </> "config" </> "int_test.json")
    case config of
      Nothing -> error "Failed to read inttest config"
      Just config' -> do
        let connStr = dataDir </> "devres" </> "int_test.db"
        catch (removeFile connStr) $ \e ->
          if isDoesNotExistError e then pure () else throw e
        mainAsWai (set otherConfig_dbConnStr (T.pack connStr) config') $
          Db.withConnection populateTestData

spec :: Spec
spec = with app $
    describe "ApiGetDirList" $
      it "gets dirs that an uploader can upload to" $ do
        request methodGet "/api/filesystem/dir" [authHeader "zelinf" "abcdef"] ""
          `shouldRespondWith` 200

authHeader :: UserName -> Password -> Header
authHeader (UserName username) (Password password) = ("Authorization", value)
  where
    value = "Basic " <> Base64.encode (T.encodeUtf8 $ username <> ":" <> password)
