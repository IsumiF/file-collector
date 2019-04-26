{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module FileCollector.Backend.Database.Class.MonadConnection
  ( MonadConnection(..)
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Pool (withResource)
import Database.Persist.Sql (SqlBackend)

import FileCollector.Backend.App (App, appEnv_sqlConnPool)

class Monad m => MonadConnection m where
  type Backend m :: *

  withConnection :: ReaderT (Backend m) m a -> m a

instance MonadConnection App where
  type Backend App = SqlBackend

  withConnection :: ReaderT SqlBackend App a -> App a
  withConnection action = do
    connPool <- (view appEnv_sqlConnPool) <$> ask
    withResource connPool (runReaderT action)
