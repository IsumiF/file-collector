{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadReadUser
  ( MonadReadUser (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist (entityKey, entityVal, get, getBy)
import Database.Persist.Sql (SqlBackend)

import FileCollector.Backend.Database.Class.Internal.Prelude

class Monad m => MonadReadUser m where
  getUserByName :: Text -> m (Maybe User)
  getUserById :: UserId -> m (Maybe User)
  getIdByUserName :: Text -> m (Maybe UserId)

instance MonadIO m => MonadReadUser (ReaderT SqlBackend m) where
  getUserByName name = do
    maybeEntityUser <- getBy (UniqueUserName name)
    pure $ fmap entityVal maybeEntityUser

  getUserById = get

  getIdByUserName name = (fmap . fmap) entityKey (getBy (UniqueUserName name))

instance MonadReadUser m =>
    MonadReadUser (MaybeT m) where
  getUserByName = lift . getUserByName
  getUserById = lift . getUserById
  getIdByUserName = lift . getIdByUserName

instance MonadReadUser m =>
    MonadReadUser (ExceptT e m) where
  getUserByName = lift . getUserByName
  getUserById = lift . getUserById
  getIdByUserName = lift . getIdByUserName
