{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.DirectoryContent
  ( getDirectoryContent
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude

getDirectoryContent :: MonadSqlDb m
                    => DirectoryId
                    -> m [(FileId, File)]
getDirectoryContent dirId = liftPersist $
    (fmap . fmap) entityToTuple (selectList [FileDirectory ==. dirId] [])
