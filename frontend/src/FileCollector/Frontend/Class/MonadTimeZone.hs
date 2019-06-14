{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.MonadTimeZone
  ( MonadTimeZone(..)
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Time
import Reflex.Dom

import FileCollector.Frontend.AppEnv

class (Reflex t, Monad m) => MonadTimeZone t m where
  getCurrentTimeZone :: m (Dynamic t TimeZone)

instance MonadWidget t m => MonadTimeZone t (ReaderT (AppEnv t m) m) where
  getCurrentTimeZone = asks (^. appEnv_timeZone)
