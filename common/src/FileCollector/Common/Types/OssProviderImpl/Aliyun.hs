{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module FileCollector.Common.Types.OssProviderImpl.Aliyun
  ( Aliyun
  , AliyunCredential(..)
  , AliyunFileLocation(..)
  ) where

import           Data.Aeson
import           Data.Text                              (Text)

import           FileCollector.Common.Types.OssProvider

data Aliyun = Aliyun

newtype AliyunCredential = AliyunCredential Text
  deriving (FromJSON, ToJSON)

newtype AliyunFileLocation = AliyunFileLocation Text
  deriving (FromJSON, ToJSON)

instance OssProvider Aliyun where
  type Credential Aliyun = AliyunCredential
  type FileLocation Aliyun = AliyunFileLocation
