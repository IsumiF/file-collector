{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.Service.MonadGetUser
  ( MonadGetUser(..)
  ) where

import FileCollector.Frontend.Class.Service.Prelude

class Monad m => MonadGetUser t m where
  getUser :: Dynamic t (Maybe BasicAuthData)
          -> Dynamic t (Either Text UserName)
          -> Event t ()
          -> m (Event t (ReqResult () User))

instance (Monad m, Reflex t) => MonadGetUser t (ReaderT (AppEnv t m) m) where
  getUser a b c = do
    f <- view (appEnv_serviceAccessors . serviceAccessors_getUser)
    lift $ f a b c
