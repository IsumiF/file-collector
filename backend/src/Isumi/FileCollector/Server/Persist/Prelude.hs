{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Isumi.FileCollector.Server.Persist.Prelude
  ( IsDbOp
  , module Database.Persist
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Database.Persist
import Database.Persist.Sql (SqlBackend)

type IsDbOp m = (MonadIO m, MonadReader SqlBackend m, MonadLogger m)

