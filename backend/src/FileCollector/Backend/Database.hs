module FileCollector.Backend.Database
  ( initialize
  ) where

import Database.Persist.Sql (runMigration)

import FileCollector.Backend.App (App)
import FileCollector.Backend.Database.Class.MonadConnection (withConnection)
import FileCollector.Backend.Database.Types.Internal (migrateAll)

initialize :: App ()
initialize = withConnection $ runMigration migrateAll
