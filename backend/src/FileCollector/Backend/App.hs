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
  , appEnv_sqlConnPool
  ) where

import Control.Lens
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.Trans.Control
    (MonadBaseControl, StM, liftBaseWith, restoreM)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)

data AppEnv = AppEnv
  { _appEnv_sqlConnPool :: Pool SqlBackend
  }

makeAppEnv :: Pool SqlBackend
           -> AppEnv
makeAppEnv = AppEnv

makeLenses ''AppEnv

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  } deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader AppEnv
      , MonadBase IO
      )

runApp :: App a -> AppEnv -> IO a
runApp (App app) = runReaderT app

instance MonadBaseControl IO App where
  type StM App a = a
  liftBaseWith f = App $ liftBaseWith $ \q -> f (q . unApp)
  restoreM = App . restoreM
