{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module FileCollector.Backend.Database.Class.MonadConnection
  ( MonadConnection(..)
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Pool (withResource)
import           Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as Persist
    (transactionSave, transactionUndo)

import FileCollector.Backend.App (App, appEnv_sqlConnPool)

class Monad m => MonadConnection m where
  type Backend m :: *

  withConnection :: ReaderT (Backend m) m a -> m a

  transactionUndo :: ReaderT (Backend m) m ()

  transactionSave :: ReaderT (Backend m) m ()

instance MonadConnection App where
  type Backend App = SqlBackend

  withConnection :: ReaderT SqlBackend App a -> App a
  withConnection action = do
    connPool <- asks (view appEnv_sqlConnPool)
    withResource connPool (runReaderT action)

  transactionUndo :: ReaderT SqlBackend App ()
  transactionUndo = Persist.transactionUndo

  transactionSave :: ReaderT SqlBackend App ()
  transactionSave = Persist.transactionSave

instance (MonadConnection m) => MonadConnection (ExceptT e m) where
  type Backend (ExceptT e m) = Backend m

  withConnection :: ReaderT (Backend m) (ExceptT e m) a -> ExceptT e m a
  withConnection action = ExceptT $ withConnection $ do
    conn <- ask
    lift $ runExceptT (runReaderT action conn)

  transactionUndo :: ReaderT (Backend m) (ExceptT e m) ()
  transactionUndo = do
    conn <- ask
    lift . lift $ runReaderT transactionUndo conn

  transactionSave :: ReaderT (Backend m) (ExceptT e m) ()
  transactionSave = do
    conn <- ask
    lift . lift $ runReaderT transactionSave conn
