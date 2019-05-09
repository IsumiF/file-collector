{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadWriteDirectory
  ( MonadWriteDirectory(..)
  , module FileCollector.Backend.Database.Class.MonadReadDirectory
  ) where

import FileCollector.Backend.Database.Class.Internal.Prelude
import FileCollector.Backend.Database.Class.MonadReadDirectory
import FileCollector.Backend.Database.Types.Directory
import qualified FileCollector.Backend.Database.Impl.WriteDirectory as Impl

class MonadReadDirectory m => MonadWriteDirectory m where
  updateDirectory :: DirectoryId
                  -> Directory -- ^new directory properties
                  -> m ()

instance MonadIO m => MonadWriteDirectory (ReaderT SqlBackend m) where
  updateDirectory = Impl.updateDirectory
