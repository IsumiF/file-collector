{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Backend.Database.Spec.Util
  ( withSqliteInMemory
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (withSqliteConn)

import           FileCollector.Backend.Database.Types.Internal (migrateAll)

withSqliteInMemory :: ReaderT SqlBackend (LoggingT IO) a -> IO a
withSqliteInMemory action =
    runStdoutLoggingT $ withSqliteConn ":memory:" (runReaderT action')
  where
    action' = runMigration migrateAll >> action
