{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module FileCollector.Backend.Test.Util
  ( WaiSessionWithArg(..)
  , MonadWaiSession(..)
  , shouldRespondWith
  , shouldRespondWith'
  , authHeader
  , contentTypeJson
  , defaultApp
  , matchJSON
  , jsonRequest
  , emptyBody
  ) where

import           Control.Exception (catch, throw)
import           Control.Lens
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           GHC.Stack (HasCallStack)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai (Application)
import           Network.Wai.Test (SResponse)
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (WriteMode), withFile)
import           System.IO.Error (isDoesNotExistError)
import           Test.Hspec.Core.Spec
    (Example (..), Params, ProgressCallback, Result (..),
    ResultStatus (Success))
import           Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai as HspecWai (shouldRespondWith)
import           Test.Hspec.Wai.Matcher
    (MatchBody (..), ResponseMatcher (..), (<:>))

import           FileCollector.Backend.Config
import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
import           FileCollector.Backend.Main (mainAsWai)
import           FileCollector.Backend.TestData.Simple (populateTestData)
import           FileCollector.Common.Types
import           Paths_file_collector_backend (getDataDir)

authHeader :: UserName -> Password -> Header
authHeader (UserName username) (Password password) = ("Authorization", value)
  where
    value = "Basic " <> Base64.encode (T.encodeUtf8 $ username <> ":" <> password)

contentTypeJson :: Header
contentTypeJson = ("Content-Type", "application/json;charset=utf-8")

defaultApp :: IO (Application, ())
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
          else
            Just $ "Expected: \n" <> lbsToString (Aeson.encode value')
              <> "\n" <> "Actual: " <> lbsToString body
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

lbsToString :: LBS.ByteString -> String
lbsToString = T.unpack . T.decodeUtf8 . LBS.toStrict

newtype WaiSessionWithArg r a =
  WaiSessionWithArg (ReaderT r WaiSession a)

type WaiExpectationWithArg r = WaiSessionWithArg r ()

deriving instance Functor (WaiSessionWithArg r)
deriving instance Applicative (WaiSessionWithArg r)
deriving instance Monad (WaiSessionWithArg r)
deriving instance MonadIO (WaiSessionWithArg r)
deriving instance MonadFail (WaiSessionWithArg r)

instance Example (WaiExpectationWithArg r) where
  type Arg (WaiExpectationWithArg r) = (Application, r)
  evaluateExample :: WaiExpectationWithArg r
                  -> Params
                  -> (((Application, r) -> IO ()) -> IO ())
                  -> ProgressCallback
                  -> IO Result
  evaluateExample (WaiSessionWithArg m) params f progressCallback = do
    resultRef <- newIORef $ Result "" Success
    let action (app, r) = do
          result <- evaluateExample (runReaderT m r) params ($ app) progressCallback
          writeIORef resultRef result
    f action
    readIORef resultRef

instance MonadReader r (WaiSessionWithArg r) where
  ask = WaiSessionWithArg ask
  local f (WaiSessionWithArg m) = WaiSessionWithArg (local f m)

class Monad m => MonadWaiSession m where
  liftWaiSession :: WaiSession a -> m a

instance MonadWaiSession (WaiSessionWithArg r) where
  liftWaiSession m = WaiSessionWithArg (lift m)

instance MonadWaiSession WaiSession where
  liftWaiSession = id

shouldRespondWith :: (HasCallStack, MonadWaiSession m)
                  => WaiSession SResponse -> ResponseMatcher -> m ()
shouldRespondWith resp respMatcher = liftWaiSession $
    HspecWai.shouldRespondWith resp respMatcher

shouldRespondWith' :: HasCallStack
                   => WaiSession SResponse -> ResponseMatcher -> WaiSessionWithArg r ()
shouldRespondWith' = shouldRespondWith
