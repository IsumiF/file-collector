{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module FileCollector.Backend.Oss.Class.MonadOssService
  ( MonadOssService (..)
  , FileName
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           FileCollector.Backend.App (App)
import qualified FileCollector.Backend.Oss.Aliyun.Internal as Aliyun
import           FileCollector.Backend.Oss.Class.MonadReadAliyunConfig
import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types.OssProvider
import           FileCollector.Common.Types.OssProviderImpl.Aliyun

type family FileName provider = r | r -> provider

class (Monad m, OssProvider provider, Convertible ByteString (FileName provider)) => MonadOssService provider m where
  getUploadCredential :: FileName provider -> m (OssClientCredential provider)
  getDownloadCredential :: FileName provider -> m (OssClientCredential provider)
  deleteFile :: FileName provider -> m Bool

type instance FileName Aliyun = AliyunFileName

instance MonadOssService Aliyun App where
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

newtype AliyunFileName = AliyunFileName Text

instance Convertible ByteString AliyunFileName where
  convert rawFileName = AliyunFileName $ T.decodeUtf8 rawFileName

instance (MonadOssService provider m, MonadTrans t, Monad (t m))
  => MonadOssService provider (t m) where
  getUploadCredential = lift . getUploadCredential
  getDownloadCredential = lift . getDownloadCredential
  deleteFile = lift . deleteFile
