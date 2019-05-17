{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadReadFile
  ( MonadReadFile(..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.ReadFile as Impl

class Monad m => MonadReadFile m where
  getFileId :: Text -- ^owner name
            -> Text -- ^directory name
            -> Text -- ^uploader name
            -> Text -- ^file name
            -> m (Maybe FileId)
  getFileById :: FileId -> m (Maybe File)

instance MonadIO m => MonadReadFile (ReaderT SqlBackend m) where
  getFileId = Impl.getFileId
  getFileById = Impl.getFileById

instance (MonadReadFile m) => (MonadReadFile (MaybeT m)) where
  getFileId a b c d = lift $ getFileId a b c d
  getFileById = lift . getFileById

instance (MonadReadFile m) => (MonadReadFile (ExceptT e m)) where
  getFileId a b c d = lift $ getFileId a b c d
  getFileById = lift . getFileById
