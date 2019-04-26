{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadUser
  ( MonadUser (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist (entityVal, getBy)
import Database.Persist.Sql (SqlBackend)

import FileCollector.Backend.Database.Types.User (Unique (UniqueName), User)

class Monad m => MonadUser m where
  getUserByName :: Text -> m (Maybe User)

instance MonadIO m => MonadUser (ReaderT SqlBackend m) where
  getUserByName name = do
    maybeEntityUser <- getBy (UniqueName name)
    pure $ fmap entityVal maybeEntityUser
