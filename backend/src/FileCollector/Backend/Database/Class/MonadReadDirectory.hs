{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module FileCollector.Backend.Database.Class.MonadReadDirectory
  ( MonadReadDirectory(..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.ReadDirectory as Impl
import           FileCollector.Backend.Database.Types.Directory

class Monad m => MonadReadDirectory m where
  getDirectory :: Text -- ^owner name
               -> Text -- ^directory name
               -> m (Maybe Directory) -- ^resulting directory
  getDirectoryId :: Text -- ^owner name
                 -> Text -- ^directory name
                 -> m (Maybe DirectoryId)
  getOwnDirectories :: Text -- ^owner name
                    -> m [Directory]
  getVisibleDirectories :: Text -- ^uploader name
                        -> m [Directory]
  getAllDirectories :: m [Directory]
  isDirectoryVisible :: Text -- ^ uploader name
                     -> DirectoryId -- ^ Directory Id
                     -> m Bool -- ^ whether the directory is visible to the uploader

instance MonadIO m => MonadReadDirectory (ReaderT SqlBackend m) where
  getDirectory = Impl.getDirectory
  getDirectoryId = Impl.getDirectoryId
  getOwnDirectories = Impl.getOwnDirectories
  getVisibleDirectories = Impl.getVisibleDirectories
  getAllDirectories = Impl.getAllDirectories
  isDirectoryVisible = Impl.isDirectoryVisible
