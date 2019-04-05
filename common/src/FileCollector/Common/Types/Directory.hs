{-# LANGUAGE DeriveGeneric #-}

module FileCollector.Common.Types.Directory
  ( Directory(..)
  , UploadRule(..)
  ) where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           GHC.Generics

data Directory = Directory
  { name           :: Text
  , ownerId        :: Text
  , expirationTime :: UTCTime
  , uploadRules    :: [UploadRule]
  } deriving (Generic, Show)

data UploadRule =
    MaxFileSize Int
  | FileNameFormat Text
  | MaxFiles Int
    deriving (Generic, Show)

instance ToJSON Directory where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Directory

instance ToJSON UploadRule where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UploadRule
