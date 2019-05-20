{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadWriteFile
  ( MonadWriteFile(..)
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.WriteFile as Impl

class Monad m => MonadWriteFile m where
  deleteFile :: FileId -> m ()
  addFile :: File -> m FileId
  updateFile :: FileId -> File -> m ()

instance MonadIO m => MonadWriteFile (ReaderT SqlBackend m) where
  deleteFile = Impl.deleteFile
  addFile = Impl.addFile
  updateFile = Impl.updateFile

instance MonadWriteFile m => MonadWriteFile (MaybeT m) where
  deleteFile = lift . deleteFile
  addFile = lift . addFile
  updateFile a b = lift $ updateFile a b

instance MonadWriteFile m => MonadWriteFile (ExceptT e m) where
  deleteFile = lift . deleteFile
  addFile = lift . addFile
  updateFile a b = lift $ updateFile a b
