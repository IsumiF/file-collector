module FileCollector.Backend.IO.Time.Class.MonadCurrentTime
  ( MonadCurrentTime (..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Time (UTCTime, getCurrentTime)

import FileCollector.Backend.App (App)

class Monad m => MonadCurrentTime m where
  currentTime :: m UTCTime

instance MonadCurrentTime App where
  currentTime = liftIO getCurrentTime

instance MonadCurrentTime m => MonadCurrentTime (ReaderT r m) where
  currentTime = lift currentTime

instance MonadCurrentTime m => MonadCurrentTime (MaybeT m) where
  currentTime = lift currentTime

instance MonadCurrentTime m => MonadCurrentTime (ExceptT e m) where
  currentTime = lift currentTime
