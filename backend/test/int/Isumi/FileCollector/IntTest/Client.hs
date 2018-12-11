{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.IntTest.Client
  ( userRole
  , module Servant.Client
  , module Servant.API
  , module Isumi.FileCollector.Api
  , ThreadId
  , startTempServer
  , shutdownTempServer
  ) where

import           Control.Concurrent (ThreadId, forkIO, throwTo, threadDelay)
import           Control.Exception.Base (AsyncException (UserInterrupt))
import           Data.Default (def)
import           Data.Proxy
import qualified Data.Text as Text
import           Isumi.FileCollector.Api
import           Isumi.FileCollector.Server
    (mainWith, serverConfigDbPoolSize, serverConfigDbString,
    serverConfigPort)
import           Network.Wai.Handler.Warp (Port)
import           Servant.API
import           Servant.Client
import           System.IO.Temp (emptySystemTempFile)

userRole :: BasicAuthData -> ClientM (Maybe Role)
userRole = client (Proxy :: Proxy Api)

startTempServer :: Port -> (FilePath -> IO ()) -> IO ThreadId
startTempServer port dbGen = do
    tempDb <- emptySystemTempFile "file-collector-inttest-db"
    dbGen tempDb
    threadId <- forkIO $ do
      mainWith def
        { serverConfigDbPoolSize = 10
        , serverConfigDbString = Text.pack tempDb
        , serverConfigPort = port
        }
    pure threadId

shutdownTempServer :: ThreadId -> IO ()
shutdownTempServer threadId = do
    throwTo threadId UserInterrupt
    threadDelay 500
