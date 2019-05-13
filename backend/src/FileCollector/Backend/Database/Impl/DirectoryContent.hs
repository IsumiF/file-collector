{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.DirectoryContent
  ( getDirectoryContent
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude
import FileCollector.Backend.Database.Types.Directory
import FileCollector.Backend.Database.Types.File

getDirectoryContent :: MonadSqlDb m
                    => DirectoryId
                    -> m [(FileId, File)]
getDirectoryContent dirId = liftPersist $
    (fmap . fmap) entityToTuple (selectList [FileDirectory ==. dirId] [])
