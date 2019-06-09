{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module FileCollector.Frontend.Class.Service.MonadDirectory
  ( MonadDirectory(..)
  ) where

import FileCollector.Frontend.AppEnv
import FileCollector.Frontend.Class.Service.Prelude
import FileCollector.Frontend.Types.LoggedUser

class (Reflex t, Monad m) => MonadDirectory t m where
  getDirList :: Event t ()
             -> m (Event t (ReqResult () [Directory]))
  getDir :: Dynamic t (Either Text UserName)
         -> Dynamic t (Either Text DirectoryName)
         -> Event t ()
         -> m (Event t (ReqResult () Directory))
  putDir :: Dynamic t (Either Text UserName)
         -> Dynamic t (Either Text DirectoryName)
         -> Dynamic t (Either Text Directory)
         -> Event t ()
         -> m (Event t (ReqResult () ()))
  deleteDir :: Dynamic t (Either Text UserName)
            -> Dynamic t (Either Text DirectoryName)
            -> Event t ()
            -> m (Event t (ReqResult () DirectoryDeleteResult))

instance (Reflex t, Monad m) => MonadDirectory t (ReaderT (AppEnv t m) m) where
  getDirList a = genImpl serviceAccessors_getDirList ($ a)
  getDir a b c = genImpl serviceAccessors_getDir (\f -> f a b c)
  putDir a b c d = genImpl serviceAccessors_putDir (\f -> f a b c d)
  deleteDir a b c = genImpl serviceAccessors_deleteDir (\f -> f a b c)

genImpl :: (Reflex t, Monad m)
        => Lens' (ServiceAccessors t m) (Dynamic t (Maybe BasicAuthData) -> f)
        -> (f -> m a)
        -> ReaderT (AppEnv t m) m a
genImpl fLens withF = do
    loggedUserDyn <- view appEnv_loggedUser
    let authDataDyn = (fmap . fmap) (view loggedUser_authData) loggedUserDyn
    f' <- view (appEnv_serviceAccessors . fLens)
    lift $ withF (f' authDataDyn)
