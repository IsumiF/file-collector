{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Backend.Database.Spec.Util
  ( withSqliteInMemory
  , withSqliteConnection
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Database.Persist.Sql (Migration, SqlBackend, runMigrationSilent)
import Database.Persist.Sqlite (withSqliteConn)

import FileCollector.Backend.Database.Types.Internal (migrateAll)

withSqliteInMemory :: LogLevel -> ReaderT SqlBackend (LoggingT IO) a -> IO a
withSqliteInMemory minLevel action =
    runStdoutLoggingT . filterLoggerLevel minLevel
      $ withSqliteConn ":memory:" (runReaderT action')
  where
    action' = runMigration' migrateAll >> action
    runMigration' = if minLevel > LevelDebug
                    then runMigrationSilent'
                    else runMigrationLog
    runMigrationSilent' migration = void (runMigrationSilent migration)

withSqliteConnection :: ReaderT SqlBackend (LoggingT IO) a -> SqlBackend -> IO a
withSqliteConnection action = runStdoutLoggingT . runReaderT action

filterLoggerLevel :: LogLevel -> LoggingT m a -> LoggingT m a
filterLoggerLevel minLevel = filterLogger $ \_ level -> level >= minLevel

runMigrationLog :: (MonadLogger m, MonadUnliftIO m) => Migration -> ReaderT SqlBackend m ()
runMigrationLog migration = do
    logs <- runMigrationSilent migration
    traverse_ $(logDebug) logs
