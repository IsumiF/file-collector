{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.Service.MonadDirectoryContent
  ( MonadDirectoryContent(..)
  ) where

import FileCollector.Frontend.Class.Service.Prelude

class (Reflex t, Monad m) => MonadDirectoryContent t m where
  getDirContent :: Dynamic t (Either Text UserName)
                -> Dynamic t (Either Text DirectoryName)
                -> Event t ()
                -> m (Event t (ReqResult () [File]))

instance (Reflex t, Monad m) => MonadDirectoryContent t (ReaderT (AppEnv t m) m) where
  getDirContent a b c =
    genericAccessor serviceAccessors_getDirContent (\f -> f a b c)
