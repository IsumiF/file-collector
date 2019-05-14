{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadDirectoryUploader
  ( MonadDirectoryUploader (..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.DirectoryUploader as Impl

class Monad m => MonadDirectoryUploader m where
  getDirUploaders :: DirectoryId -> m [UserId]

  deleteAllUploadersOfDir :: DirectoryId -> m ()
  addUploaderToDir :: DirectoryId -> UserId -> m ()

instance MonadIO m => MonadDirectoryUploader (ReaderT SqlBackend m) where
  getDirUploaders = Impl.getDirUploaders

  deleteAllUploadersOfDir = Impl.deleteAllUploadersOfDir
  addUploaderToDir = Impl.addUploaderToDir
