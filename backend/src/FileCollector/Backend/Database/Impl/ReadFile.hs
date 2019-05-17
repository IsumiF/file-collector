{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.ReadFile
  ( getFileId
  , getFileById
  ) where

import Control.Monad.Trans.Maybe

import FileCollector.Backend.Database.Impl.Internal.Prelude
import FileCollector.Backend.Database.Impl.ReadDirectory

getFileId :: MonadSqlDb m
          => Text -- ^owner name
          -> Text -- ^directory name
          -> Text -- ^uploader name
          -> Text -- ^file name
          -> m (Maybe FileId)
getFileId ownerName dirName uploaderName fileName' = runMaybeT $ do
  dirId <- MaybeT $ getDirectoryId ownerName dirName
  entityUser <- MaybeT $ liftPersist $ getBy (UniqueUserName uploaderName)
  let uploaderId = entityKey entityUser
  file <- MaybeT $ liftPersist $ getBy (UniqueFile dirId uploaderId fileName')
  pure $ entityKey file

getFileById :: MonadSqlDb m
            => FileId
            -> m (Maybe File)
getFileById fileId = liftPersist $ get fileId
