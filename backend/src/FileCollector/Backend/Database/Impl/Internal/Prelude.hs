{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.Internal.Prelude
  ( Text
  , module Control.Monad.Reader
  , module Control.Monad.IO.Class
  , module Database.Persist
  , SqlBackend
  , rawSql
  , sqlQQ
  , MonadSqlDb
  , entityToTuple
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql (SqlBackend, rawSql, sqlQQ)

type MonadSqlDb m = (MonadIO m, MonadReader SqlBackend m)

entityToTuple :: Entity record -> (Key record, record)
entityToTuple entity = (entityKey entity, entityVal entity)
