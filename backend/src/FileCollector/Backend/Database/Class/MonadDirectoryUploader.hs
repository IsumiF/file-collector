{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadDirectoryUploader
  ( MonadDirectoryUploader (..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.DirectoryUploader as Impl

class Monad m => MonadDirectoryUploader m where
  getDirUploaders :: DirectoryId -> m [UserId]
  dirHasUploader :: DirectoryId -> UserId -> m Bool

  deleteAllUploadersOfDir :: DirectoryId -> m ()
  addUploaderToDir :: DirectoryId -> UserId -> m ()

instance MonadIO m => MonadDirectoryUploader (ReaderT SqlBackend m) where
  getDirUploaders = Impl.getDirUploaders
  dirHasUploader = Impl.dirHasUploader

  deleteAllUploadersOfDir = Impl.deleteAllUploadersOfDir
  addUploaderToDir = Impl.addUploaderToDir

instance (MonadDirectoryUploader m) => (MonadDirectoryUploader (MaybeT m)) where
  getDirUploaders = lift . getDirUploaders
  dirHasUploader a b = lift $ dirHasUploader a b
  deleteAllUploadersOfDir = lift . deleteAllUploadersOfDir
  addUploaderToDir a b = lift $ addUploaderToDir a b

instance (MonadDirectoryUploader m)
  => (MonadDirectoryUploader (ExceptT e m)) where
  getDirUploaders = lift . getDirUploaders
  dirHasUploader a b = lift $ dirHasUploader a b
  deleteAllUploadersOfDir = lift . deleteAllUploadersOfDir
  addUploaderToDir a b = lift $ addUploaderToDir a b
