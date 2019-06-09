{-# LANGUAGE RankNTypes #-}

module FileCollector.Frontend.Class.Service.Prelude
  ( module Control.Lens
  , module Control.Monad.Reader
  , Text
  , module Reflex
  , BasicAuthData
  , module Servant.Reflex
  , module FileCollector.Common.Types
  , module FileCollector.Frontend.AppEnv
  , module FileCollector.Frontend.Types.ServiceAccessors
  , genericAccessor
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Text (Text)
import Reflex
import Servant.API (BasicAuthData)
import Servant.Reflex (ReqResult, QParam)

import FileCollector.Common.Types
import FileCollector.Frontend.AppEnv
import FileCollector.Frontend.Types.LoggedUser
import FileCollector.Frontend.Types.ServiceAccessors

genericAccessor :: (Reflex t, Monad m)
                => Lens' (ServiceAccessors t m) (Dynamic t (Maybe BasicAuthData) -> f)
                -> (f -> m a)
                -> ReaderT (AppEnv t m) m a
genericAccessor fLens withF = do
    loggedUserDyn <- view appEnv_loggedUser
    let authDataDyn = (fmap . fmap) (view loggedUser_authData) loggedUserDyn
    f' <- view (appEnv_serviceAccessors . fLens)
    lift $ withF (f' authDataDyn)
