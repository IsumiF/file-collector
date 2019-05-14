{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module FileCollector.Backend.Database.Class.MonadReadDirectory
  ( MonadReadDirectory(..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.ReadDirectory as Impl

class Monad m => MonadReadDirectory m where
  getDirectoryById :: DirectoryId
                   -> m (Maybe Directory)
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

{-# DEPRECATED getVisibleDirectories
  "Use 'MonadDirectoryUploader getDirUploaders' instead" #-}

{-# DEPRECATED isDirectoryVisible
  "Use 'MonadDirectoryUploader dirHasUploader' instead" #-}

instance MonadIO m => MonadReadDirectory (ReaderT SqlBackend m) where
  getDirectoryById = Impl.getDirectoryById
  getDirectory = Impl.getDirectory
  getDirectoryId = Impl.getDirectoryId
  getOwnDirectories = Impl.getOwnDirectories
  getVisibleDirectories = Impl.getVisibleDirectories
  getAllDirectories = Impl.getAllDirectories
  isDirectoryVisible = Impl.isDirectoryVisible

instance MonadReadDirectory m
  => MonadReadDirectory (MaybeT m) where
  getDirectoryById = lift . getDirectoryById
  getDirectory a b = lift $ getDirectory a b
  getDirectoryId a b = lift $ getDirectoryId a b
  getOwnDirectories = lift . getOwnDirectories
  getVisibleDirectories = lift . getVisibleDirectories
  getAllDirectories = lift getAllDirectories
  isDirectoryVisible a b = lift $ isDirectoryVisible a b

instance MonadReadDirectory m
  => MonadReadDirectory (ExceptT e m) where
  getDirectoryById = lift . getDirectoryById
  getDirectory a b = lift $ getDirectory a b
  getDirectoryId a b = lift $ getDirectoryId a b
  getOwnDirectories = lift . getOwnDirectories
  getVisibleDirectories = lift . getVisibleDirectories
  getAllDirectories = lift getAllDirectories
  isDirectoryVisible a b = lift $ isDirectoryVisible a b
