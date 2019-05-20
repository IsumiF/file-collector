{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.WriteFile
  ( deleteFile
  , addFile
  , updateFile
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude

deleteFile :: MonadSqlDb m
           => FileId
           -> m ()
deleteFile fileId = liftPersist $ delete fileId

addFile :: MonadSqlDb m
        => File
        -> m FileId
addFile = liftPersist . insert

updateFile :: MonadSqlDb m
           => FileId
           -> File
           -> m ()
updateFile fildId file = liftPersist $ repsert fildId file
