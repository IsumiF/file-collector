module FileCollector.Backend.IO.UUID.Class.MonadUUID
  ( MonadUUID(..)
  , UUID
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.UUID (UUID)
import qualified Data.UUID.V1 as UUID

import FileCollector.Backend.App (App)

class Monad m => MonadUUID m where
  nextUUID :: m (Maybe UUID)

instance MonadUUID App where
  nextUUID = liftIO UUID.nextUUID

instance MonadUUID m => MonadUUID (ReaderT r m) where
  nextUUID = lift nextUUID

instance MonadUUID m => MonadUUID (MaybeT m) where
  nextUUID = lift nextUUID

instance MonadUUID m => MonadUUID (ExceptT e m) where
  nextUUID = lift nextUUID
