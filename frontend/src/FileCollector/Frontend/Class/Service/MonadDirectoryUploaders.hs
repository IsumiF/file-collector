{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.Service.MonadDirectoryUploaders
  ( MonadDirectoryUploaders(..)
  ) where

import FileCollector.Frontend.Class.Service.Prelude

class (Reflex t, Monad m) => MonadDirectoryUploaders t m where
  getDirUploaders :: Dynamic t (Either Text UserName)
                  -> Dynamic t (Either Text DirectoryName)
                  -> Event t ()
                  -> m (Event t (ReqResult () [UserName]))
  putDirUploaders :: Dynamic t (Either Text UserName)
                  -> Dynamic t (Either Text DirectoryName)
                  -> Dynamic t (Either Text [UserName])
                  -> Event t ()
                  -> m (Event t (ReqResult () ()))

instance (Reflex t, Monad m) => MonadDirectoryUploaders t (ReaderT (AppEnv t m) m) where
  getDirUploaders a b c =
    genericAccessor serviceAccessors_getDirUploaders (\f -> f a b c)
  putDirUploaders a b c d =
    genericAccessor serviceAccessors_putDirUploaders (\f -> f a b c d)
