{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module FileCollector.Frontend.Class.Service.MonadDirectory
  ( MonadDirectory(..)
  ) where

import FileCollector.Frontend.Class.Service.Prelude

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
  getDirList a = genericAccessor serviceAccessors_getDirList ($ a)
  getDir a b c = genericAccessor serviceAccessors_getDir (\f -> f a b c)
  putDir a b c d = genericAccessor serviceAccessors_putDir (\f -> f a b c d)
  deleteDir a b c = genericAccessor serviceAccessors_deleteDir (\f -> f a b c)
