{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module FileCollector.Common.Types.OssProviderImpl.Aliyun
  ( Aliyun(..)
  , OssClientCredential(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import FileCollector.Common.Types.OssProvider

data Aliyun = Aliyun

instance OssProvider Aliyun where
  data OssClientCredential Aliyun = AliyunSignedUrl Text

deriving instance Generic (OssClientCredential Aliyun)

instance FromJSON (OssClientCredential Aliyun) where

instance ToJSON (OssClientCredential Aliyun) where
  toEncoding = genericToEncoding defaultOptions
