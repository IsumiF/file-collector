{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module FileCollector.Backend.Oss.Class.MonadOssService
  ( MonadOssService (..)
  , FileName
  , HasError(..)
  , FromRawPath(..)
  ) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Text (Text)

import           FileCollector.Backend.App (App)
import qualified FileCollector.Backend.Oss.Impl.Aliyun as Impl
import qualified FileCollector.Common.Types as Common

type family FileName provider = r | r -> provider

class
  ( Monad m
  , HasError provider
  , FromRawPath (FileName provider)
  , Common.OssProvider provider
  ) => MonadOssService provider m where
  -- TODO 增加返回错误信息的功能
  getUploadCredential :: FileName provider -> m (Either (Error provider) (Common.OssClientCredential provider))
  getDownloadCredential :: FileName provider -> m (Either (Error provider) (Common.OssClientCredential provider))
  deleteFile :: FileName provider -> m Bool

class FromRawPath fileName where
  fromRawPath :: ByteString -> fileName

class HasError provider where
  type Error provider = r | r -> provider
  toErrorMessage :: (Error provider) -> Text

instance (MonadOssService provider m, MonadTrans t, Monad (t m))
  => MonadOssService provider (t m) where
  getUploadCredential = lift . getUploadCredential
  getDownloadCredential = lift . getDownloadCredential
  deleteFile = lift . deleteFile

-- instances for Aliyun.

type instance FileName Common.Aliyun = Impl.AliyunFileName

instance HasError Common.Aliyun where
  type Error Common.Aliyun = Impl.AliyunError
  toErrorMessage Impl.AliyunError = "Failure"

instance FromRawPath Impl.AliyunFileName where
  fromRawPath = Impl.fromRawPath

instance MonadOssService Common.Aliyun App where
  getUploadCredential = Impl.getUploadCredential
  getDownloadCredential = Impl.getDownloadCredential
  deleteFile = Impl.deleteFile
