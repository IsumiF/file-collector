{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadDirectoryContent
  ( MonadDirectoryContent(..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.DirectoryContent as Impl

class Monad m => MonadDirectoryContent m where
  getDirectoryContent :: DirectoryId -> m [(FileId, File)]
  getDirectoryContentOfUploader :: DirectoryId -> UserId -> m [(FileId, File)]

instance MonadIO m => MonadDirectoryContent (ReaderT SqlBackend m) where
  getDirectoryContent = Impl.getDirectoryContent
  getDirectoryContentOfUploader = Impl.getDirectoryContentOfUploader

instance MonadDirectoryContent m
  => MonadDirectoryContent (MaybeT m) where
  getDirectoryContent = lift . getDirectoryContent
  getDirectoryContentOfUploader a b = lift $ getDirectoryContentOfUploader a b

instance MonadDirectoryContent m
  => MonadDirectoryContent (ExceptT e m) where
  getDirectoryContent = lift . getDirectoryContent
  getDirectoryContentOfUploader a b = lift $ getDirectoryContentOfUploader a b
