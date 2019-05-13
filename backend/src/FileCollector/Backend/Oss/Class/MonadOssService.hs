{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FileCollector.Backend.Oss.Class.MonadOssService
  ( MonadOssService (..)
  , FileName(AliyunFileName)
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)

import FileCollector.Common.Types.OssProvider
import FileCollector.Common.Types.OssProviderImpl.Aliyun
import FileCollector.Backend.Oss.Class.MonadReadAliyunConfig
import qualified FileCollector.Backend.Oss.Aliyun.Internal as Aliyun

class (Monad m, OssProvider provider) => MonadOssService provider m where
  data FileName provider :: *
  getUploadCredential :: FileName provider -> m (OssClientCredential provider)
  getDownloadCredential :: FileName provider -> m (OssClientCredential provider)
  deleteFile :: FileName provider -> m Bool

instance (MonadIO m, MonadReadAliyunConfig m) => MonadOssService Aliyun m where
  data FileName Aliyun = AliyunFileName Text
  getUploadCredential (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    AliyunSignedUrl <$> liftIO (Aliyun.getUploadUrl accessKey objId)
  getDownloadCredential (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    AliyunSignedUrl <$> liftIO (Aliyun.getDownloadUrl accessKey objId)
  deleteFile (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    liftIO $ Aliyun.deleteFile accessKey objId
