{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.DirectoryContent
  ( getDirectoryContent
  , getDirectoryContentOfUploader
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude

getDirectoryContent :: MonadSqlDb m
                    => DirectoryId
                    -> m [(FileId, File)]
getDirectoryContent dirId = liftPersist $
    (fmap . fmap) entityToTuple (selectList [FileDirectory ==. dirId] [])

getDirectoryContentOfUploader ::
     MonadSqlDb m
  => DirectoryId
  -> UserId
  -> m [(FileId, File)]
getDirectoryContentOfUploader dirId userId = liftPersist $ do
    es <- selectList [FileUploader ==. userId, FileDirectory ==. dirId] []
    pure $ fmap entityToTuple es
