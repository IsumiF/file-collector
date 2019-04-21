{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Isumi.FileCollector.Server.Persist.Prelude
  ( Database
  , module Database.Persist
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Database.Persist
import Database.Persist.Sql (SqlBackend)

type Database m = (MonadIO m, MonadReader SqlBackend m, MonadLogger m)

