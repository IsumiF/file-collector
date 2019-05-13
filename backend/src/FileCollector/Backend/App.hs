{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module FileCollector.Backend.App
  ( App
  , runApp
  , AppEnv
  , makeAppEnv
  -- *Lens
  -- **AppEnv
  , appEnv_sqlConnPool
  , appEnv_logger
  , appEnv_oss
  ) where

import Control.Lens
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
    (MonadBaseControl, StM, liftBaseWith, restoreM)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)

import FileCollector.Backend.Config (ConfigOss)

import FileCollector.Backend.Logger (Logger)

-- | The environment stored in 'App' monad
data AppEnv = AppEnv
  { _appEnv_sqlConnPool :: Pool SqlBackend
  , _appEnv_logger      :: Logger
  , _appEnv_oss         :: ConfigOss
  }

-- | Construct new 'AppEnv'
makeAppEnv :: Pool SqlBackend -- ^SQL connection pool
           -> Logger -- ^logger
           -> ConfigOss
           -> AppEnv
makeAppEnv = AppEnv

makeLenses ''AppEnv

-- | The concrete monad our server application code actually runs in.
-- It's basically a 'ReaderT' over 'IO'
newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  } deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader AppEnv
      , MonadBase IO
      , MonadUnliftIO
      )

-- | Run 'App' monad in 'IO'
runApp :: App a
       -> AppEnv -- ^initial environment
       -> IO a
runApp (App app) = runReaderT app

instance MonadBaseControl IO App where
  type StM App a = a
  liftBaseWith f = App $ liftBaseWith $ \q -> f (q . unApp)
  restoreM = App . restoreM

instance MonadLogger App where
  monadLoggerLog loc source level msg = do
    logger <- asks (view appEnv_logger)
    liftIO $ logger loc source level (toLogStr msg)
