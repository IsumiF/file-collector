{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module FileCollector.Backend.Oss.Class.MonadOssService
  ( MonadOssService (..)
  , FileName
  , HasError(..)
  , FromRawFileName(..)
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           FileCollector.Backend.App (App)
import qualified FileCollector.Backend.Oss.Aliyun.Internal as Aliyun
import           FileCollector.Backend.Oss.Class.MonadReadAliyunConfig
import           FileCollector.Common.Types.OssProvider
import           FileCollector.Common.Types.OssProviderImpl.Aliyun

type family FileName provider = r | r -> provider

class
  ( Monad m
  , HasError provider
  , OssProvider provider
  , FromRawFileName (FileName provider)
  ) => MonadOssService provider m where
  -- TODO 增加返回错误信息的功能
  getUploadCredential :: FileName provider -> m (Either (Error provider) (OssClientCredential provider))
  getDownloadCredential :: FileName provider -> m (Either (Error provider) (OssClientCredential provider))
  deleteFile :: FileName provider -> m Bool

class HasError provider where
  type Error provider = r | r -> provider
  toErrorMessage :: (Error provider) -> Text

class FromRawFileName fileName where
  fromRawFileName :: ByteString -> fileName

instance (MonadOssService provider m, MonadTrans t, Monad (t m))
  => MonadOssService provider (t m) where
  getUploadCredential = lift . getUploadCredential
  getDownloadCredential = lift . getDownloadCredential
  deleteFile = lift . deleteFile

type instance FileName Aliyun = AliyunFileName

-- instances for Aliyun begins.

instance HasError Aliyun where
  type Error Aliyun = AliyunError
  toErrorMessage AliyunError = "Failure"

instance MonadOssService Aliyun App where
  getUploadCredential (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    cred <- AliyunSignedUrl <$> liftIO (Aliyun.getUploadUrl accessKey objId)
    pure $ Right cred
  getDownloadCredential (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    cred <- AliyunSignedUrl <$> liftIO (Aliyun.getDownloadUrl accessKey objId)
    pure $ Right cred
  deleteFile (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    liftIO $ Aliyun.deleteFile accessKey objId

newtype AliyunFileName = AliyunFileName Text

data AliyunError = AliyunError

instance FromRawFileName AliyunFileName where
  fromRawFileName rawFileName = AliyunFileName $ T.decodeUtf8 rawFileName

-- instances for Aliyun ends.
