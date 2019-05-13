{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.WriteFile
  ( deleteFile
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude
import FileCollector.Backend.Database.Types.File

deleteFile :: MonadSqlDb m
           => FileId
           -> m ()
deleteFile fileId = liftPersist $ delete fileId
