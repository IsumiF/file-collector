{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

module FileCollector.Backend.Database
  ( initialize
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Database.Persist.Class (liftPersist)
import Database.Persist.Sql (SqlBackend, runMigrationSilent)

import FileCollector.Backend.Database.Types.Internal (migrateAll)

initialize ::
  ( MonadLogger m
  , MonadIO m
  , MonadUnliftIO m
  , MonadReader SqlBackend m
  )
  => m ()
initialize = do
    logs <- liftPersist $ runMigrationSilent migrateAll
    traverse_ $(logDebug) logs
