{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Isumi.FileCollector.IntTest.Client
  ( userRole
  , module Servant.Client
  , module Servant.API
  , module Isumi.FileCollector.Api
  , withTempServer
  , runClientM'
  , runReaderT
  , lift
  ) where

import           Control.Concurrent.Async (Async, async, cancelWith, waitCatch)
import           Control.Exception.Base (AsyncException (UserInterrupt))
import           Control.Monad (void)
import           Control.Monad.Reader
import           Control.Monad.Trans.Class (lift)
import           Data.Default (def)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import qualified Data.Text as Text
import           Isumi.FileCollector.Api
import           Isumi.FileCollector.Server
    (mainWith, serverConfigDbPoolSize, serverConfigDbString, serverConfigPort)
import           Network.HTTP.Client
    (Manager, defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp (Port)
import           Servant.API
import           Servant.Client
import           System.Environment (lookupEnv)
import           System.IO.Temp (emptySystemTempFile)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (Spec, SpecWith, afterAll, beforeAll)
import           Text.Read (readMaybe)

userRole :: BasicAuthData -> ClientM (Maybe Role)
userRole = client (Proxy :: Proxy Api)

port :: Port
port = unsafePerformIO $ do
    envPortStr <- lookupEnv "PORT"
    pure $ fromMaybe 8081 (envPortStr >>= readMaybe)
{-# NOINLINE port #-}

startTempServer :: (FilePath -> IO ()) -> IO (Async ())
startTempServer dbGen = do
    tempDb <- emptySystemTempFile "file-collector-inttest-db"
    dbGen tempDb
    server <- async $ do
      mainWith def
        { serverConfigDbPoolSize = 10
        , serverConfigDbString = Text.pack tempDb
        , serverConfigPort = port
        }
    pure server

shutdownTempServer :: TempServer -> IO ()
shutdownTempServer server = do
    let server' = tempServerAsync server
    cancelWith server' UserInterrupt
    void $ waitCatch server'

data TempServer = TempServer
    { tempServerAsync         :: Async ()
    , tempServerClientManager :: Manager
    }

withTempServer :: (FilePath -> IO ()) -> SpecWith TempServer -> Spec
withTempServer dbGen spec =
    beforeAll (TempServer <$> startTempServer dbGen
                          <*> newManager defaultManagerSettings) $
      afterAll shutdownTempServer $ spec

runClientM' :: (MonadReader TempServer m, MonadIO m)
            => ClientM a -> m (Either ServantError a)
runClientM' c = do
    server <- ask
    liftIO $ runClientM c (clientEnv (tempServerClientManager server))

clientEnv :: Manager -> ClientEnv
clientEnv m = mkClientEnv m (BaseUrl Http "localhost" port "")

