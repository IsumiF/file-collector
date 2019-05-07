{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadUser
  ( MonadUser (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist (entityVal, get, getBy)
import Database.Persist.Sql (SqlBackend)

import FileCollector.Backend.Database.Types.User

class Monad m => MonadUser m where
  getUserByName :: Text -> m (Maybe User)
  getUserById :: UserId -> m (Maybe User)

instance MonadIO m => MonadUser (ReaderT SqlBackend m) where
  getUserByName name = do
    maybeEntityUser <- getBy (UniqueUserName name)
    pure $ fmap entityVal maybeEntityUser

  getUserById = get
