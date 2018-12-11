{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Isumi.FileCollector.IntTest.Client
  ( userRole
  , module Servant.Client
  , module Servant.API
  , module Isumi.FileCollector.Api
  , startTempServer
  , shutdownTempServer
  ) where

import           Control.Concurrent.Async (Async, async, cancelWith, waitCatch)
import           Control.Exception.Base (AsyncException (UserInterrupt))
import           Control.Monad (void)
import           Data.Default (def)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import qualified Data.Text as Text
import           Isumi.FileCollector.Api
import           Isumi.FileCollector.Server
    (mainWith, serverConfigDbPoolSize, serverConfigDbString, serverConfigPort)
import           Servant.API
import           Servant.Client
import           System.Environment (lookupEnv)
import           System.IO.Temp (emptySystemTempFile)
import           Text.Read (readMaybe)

userRole :: BasicAuthData -> ClientM (Maybe Role)
userRole = client (Proxy :: Proxy Api)

startTempServer :: (FilePath -> IO ()) -> IO (Async ())
startTempServer dbGen = do
    envPortStr <- lookupEnv "PORT"
    let envPort = envPortStr >>= readMaybe
    tempDb <- emptySystemTempFile "file-collector-inttest-db"
    dbGen tempDb
    server <- async $ do
      mainWith def
        { serverConfigDbPoolSize = 10
        , serverConfigDbString = Text.pack tempDb
        , serverConfigPort = fromMaybe 8081 envPort
        }
    pure server

shutdownTempServer :: Async () -> IO ()
shutdownTempServer server = do
    cancelWith server UserInterrupt
    void $ waitCatch server

