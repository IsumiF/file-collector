{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.WriteDirectory
  ( updateDirectory
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude
import FileCollector.Backend.Database.Types.Directory

updateDirectory :: MonadSqlDb m
                => DirectoryId
                -> Directory -- ^new directory properties
                -> m ()
updateDirectory dirId newDir =
    liftPersist $ replace dirId newDir
