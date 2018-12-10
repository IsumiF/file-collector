module Isumi.FileCollector.Server.Handler
  ( unAppHandler
  , AppHandler
  , server
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import Isumi.FileCollector.Api (Api)
import Isumi.FileCollector.Server.Handler.Prelude
import Isumi.FileCollector.Server.SqlConnPool (getSqlConnPool)

import qualified Isumi.FileCollector.Server.Handler.User as User

unAppHandler :: AppHandler a -> Handler a
unAppHandler (AppHandler appHandler) = do
    pool <- liftIO getSqlConnPool
    flip runReaderT (pool :: Pool SqlBackend) $
      runStdoutLoggingT appHandler

server :: ServerT Api AppHandler
server = User.server
