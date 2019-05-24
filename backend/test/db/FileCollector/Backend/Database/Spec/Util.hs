{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Backend.Database.Spec.Util
  ( withSqliteInMemory
  ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite (withSqliteConn)

import FileCollector.Backend.Database (initialize)

withSqliteInMemory :: LogLevel -> ReaderT SqlBackend (LoggingT IO) a -> IO a
withSqliteInMemory minLevel action =
    runStdoutLoggingT . filterLoggerLevel minLevel
      $ withSqliteConn ":memory:" (runReaderT action')
  where
    action' = initialize >> action

filterLoggerLevel :: LogLevel -> LoggingT m a -> LoggingT m a
filterLoggerLevel minLevel = filterLogger $ const (>= minLevel)
