module FileCollector.Backend.Handler.AppHandler
  ( AppHandler
  , toHandler
  , module Control.Monad.Except
  , App
  , AppEnv
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Servant.Server

import FileCollector.Backend.App (App, AppEnv, runApp)

-- | The monad that all handler code run in
type AppHandler = ExceptT ServantErr App

-- | Convert 'AppHandler' monad to Servant's 'Handler' monad
toHandler :: AppEnv -> AppHandler a -> Handler a
toHandler appEnv appHandler = do
  resultEither <- liftIO $ runApp (runExceptT appHandler) appEnv
  case resultEither of
    Left err     -> throwError err
    Right result -> pure result
