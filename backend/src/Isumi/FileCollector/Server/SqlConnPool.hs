module Isumi.FileCollector.Server.SqlConnPool
  ( getSqlConnPool
  , initSqlConnPool
  ) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import System.IO.Unsafe (unsafePerformIO)

sqlConnPool :: IORef (Maybe (Pool SqlBackend))
sqlConnPool = unsafePerformIO (newIORef Nothing)
{-# NOINLINE sqlConnPool #-}

getSqlConnPool :: IO (Pool SqlBackend)
getSqlConnPool = do
    poolM <- readIORef sqlConnPool
    case poolM of
      Nothing   -> error "sql connection pool not initialized."
      Just pool -> pure pool

initSqlConnPool :: MonadIO m => Pool SqlBackend -> m ()
initSqlConnPool pool =
    liftIO $ writeIORef sqlConnPool (Just pool)

