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

type AppHandler = ExceptT ServantErr App

toHandler :: AppEnv -> AppHandler a -> Handler a
toHandler appEnv appHandler = do
  resultEither <- liftIO $ runApp (runExceptT appHandler) appEnv
  case resultEither of
    Left err     -> throwError err
    Right result -> pure result
