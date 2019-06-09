{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.Service.MonadFile
  ( MonadFile(..)
  ) where

import FileCollector.Frontend.Class.Service.Prelude

class (Reflex t, Monad m) => MonadFile t m where
  getFile :: Dynamic t (Either Text UserName)
          -> Dynamic t (Either Text DirectoryName)
          -> Dynamic t (Either Text UserName)
          -> Dynamic t (Either Text FileName)
          -> Dynamic t Bool
          -> Event t ()
          -> m (Event t (ReqResult () (File, Maybe (OssClientCredential Aliyun))))
  putFile :: Dynamic t (Either Text UserName)
          -> Dynamic t (Either Text DirectoryName)
          -> Dynamic t (Either Text UserName)
          -> Dynamic t (Either Text FileName)
          -> Dynamic t (QParam FileName)
          -> Event t ()
          -> m (Event t (ReqResult () (OssClientCredential Aliyun)))
  commitPutFile :: Dynamic t (Either Text UserName)
                -> Dynamic t (Either Text DirectoryName)
                -> Dynamic t (Either Text UserName)
                -> Dynamic t (Either Text FileName)
                -> Event t ()
                -> m (Event t (ReqResult () ()))
  deleteFile :: Dynamic t (Either Text UserName)
             -> Dynamic t (Either Text DirectoryName)
             -> Dynamic t (Either Text UserName)
             -> Dynamic t (Either Text FileName)
             -> Event t ()
             -> m (Event t (ReqResult () ()))

instance (Reflex t, Monad m) => MonadFile t (ReaderT (AppEnv t m) m) where
  getFile x1 x2 x3 x4 x5 x6 = genericAccessor serviceAccessors_getFile $ \f -> f x1 x2 x3 x4 x5 x6
  putFile x1 x2 x3 x4 x5 x6 = genericAccessor serviceAccessors_putFile $ \f -> f x1 x2 x3 x4 x5 x6
  commitPutFile x1 x2 x3 x4 x5 = genericAccessor serviceAccessors_commitPutFile $ \f -> f x1 x2 x3 x4 x5
  deleteFile x1 x2 x3 x4 x5 =
    genericAccessor serviceAccessors_deleteFile $ \f -> f x1 x2 x3 x4 x5
