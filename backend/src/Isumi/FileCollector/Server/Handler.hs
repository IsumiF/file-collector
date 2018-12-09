module Isumi.FileCollector.Server.Handler
  ( unAppHandler
  , AppHandler
  ) where

import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Logger
    ( runStdoutLoggingT
    )
import Control.Monad.Reader
    ( runReaderT
    )
import Data.Pool
    ( Pool
    )
import Database.Persist.Sql
    ( SqlBackend
    )
import Isumi.FileCollector.Server.Handler.Prelude
import Isumi.FileCollector.Server.SqlConnPool
    ( getSqlConnPool
    )

unAppHandler :: AppHandler a -> Handler a
unAppHandler (AppHandler appHandler) = do
    pool <- liftIO getSqlConnPool
    flip runReaderT (pool :: Pool SqlBackend) $
      runStdoutLoggingT appHandler

