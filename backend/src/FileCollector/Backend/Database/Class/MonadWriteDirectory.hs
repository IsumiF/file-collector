{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadWriteDirectory
  ( MonadWriteDirectory(..)
  , module FileCollector.Backend.Database.Class.MonadReadDirectory
  ) where

import           FileCollector.Backend.Database.Class.Internal.Prelude
import           FileCollector.Backend.Database.Class.MonadReadDirectory
import qualified FileCollector.Backend.Database.Impl.WriteDirectory as Impl

class MonadReadDirectory m => MonadWriteDirectory m where
  updateDirectory :: DirectoryId
                  -> Directory -- ^new directory properties
                  -> m ()
  deleteDirectory :: DirectoryId
                  -> m ()

instance MonadIO m => MonadWriteDirectory (ReaderT SqlBackend m) where
  updateDirectory = Impl.updateDirectory

  deleteDirectory = Impl.deleteDirectory

instance MonadWriteDirectory m
  => MonadWriteDirectory (MaybeT m) where
  updateDirectory a b = lift $ updateDirectory a b
  deleteDirectory = lift . deleteDirectory

instance MonadWriteDirectory m
  => MonadWriteDirectory (ExceptT e m) where
  updateDirectory a b = lift $ updateDirectory a b
  deleteDirectory = lift . deleteDirectory
