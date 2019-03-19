{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Isumi.FileCollector.Server.Handler.Internal.Prelude
  ( module Servant
  , AppHandler(..)
  , runDbOp
  , module Isumi.FileCollector.Api
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Isumi.FileCollector.Api hiding (Api)
import Isumi.FileCollector.Server.Persist (Database)
import Servant

newtype AppHandler a = AppHandler
    (LoggingT (ReaderT (Pool SqlBackend) Handler) a)
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader (Pool SqlBackend), MonadLogger)

runDbOp
    :: forall a. (forall m. Database m => m a)
    -> AppHandler a
runDbOp op = do
    pool <- ask
    liftIO $ runSqlPool (runStdoutLoggingT op) pool
