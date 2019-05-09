{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.WriteDirectory
  ( updateDirectory
  , deleteDirectory
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude
import FileCollector.Backend.Database.Types.Directory

updateDirectory :: MonadSqlDb m
                => DirectoryId
                -> Directory -- ^new directory properties
                -> m ()
updateDirectory dirId newDir =
    liftPersist $ replace dirId newDir

deleteDirectory :: MonadSqlDb m
                => DirectoryId
                -> m ()
deleteDirectory dirId = liftPersist $ delete dirId
