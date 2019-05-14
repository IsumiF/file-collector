{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadDirectoryContent
  ( MonadDirectoryContent(..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.DirectoryContent as Impl

class Monad m => MonadDirectoryContent m where
  getDirectoryContent :: DirectoryId -> m [(FileId, File)]

instance MonadIO m => MonadDirectoryContent (ReaderT SqlBackend m) where
  getDirectoryContent = Impl.getDirectoryContent
