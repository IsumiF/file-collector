{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.DirectoryUploader
  ( deleteAllUploadersOfDir
  , addUploaderToDir
  , getDirUploaders
  ) where

import FileCollector.Backend.Database.Impl.Internal.Prelude

deleteAllUploadersOfDir :: MonadSqlDb m
                        => DirectoryId
                        -> m ()
deleteAllUploadersOfDir dirId = liftPersist $
    deleteWhere [CanUploadToDirectory ==. dirId]

addUploaderToDir :: MonadSqlDb m
                 => DirectoryId
                 -> UserId
                 -> m ()
addUploaderToDir dirId uploaderId = liftPersist $ do
    deleteWhere [CanUploadToDirectory ==. dirId, CanUploadToUser ==. uploaderId]
    insert_ $ CanUploadTo uploaderId dirId

getDirUploaders :: MonadSqlDb m
                => DirectoryId
                -> m [UserId]
getDirUploaders dirId = liftPersist $ do
    es <- selectList [CanUploadToDirectory ==. dirId] []
    pure $ fmap (canUploadToUser . entityVal) es
