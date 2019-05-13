{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FileCollector.Backend.Oss.Class.MonadOssService
  ( MonadOssService (..)
  , FileName(AliyunFileName)
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import qualified FileCollector.Backend.Oss.Aliyun.Internal as Aliyun
import           FileCollector.Backend.Oss.Class.MonadReadAliyunConfig
import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types.OssProvider
import           FileCollector.Common.Types.OssProviderImpl.Aliyun

class (Monad m, OssProvider provider, Convertible ByteString (FileName provider)) => MonadOssService provider m where
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

instance Convertible ByteString (FileName Aliyun) where
  convert rawFileName = AliyunFileName $ T.decodeUtf8 rawFileName
