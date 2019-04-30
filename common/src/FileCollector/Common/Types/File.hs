{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Common.Types.File
  ( File(..)
  -- *Lens
  , file_name
  , file_uploaderName
  , file_hash
  , file_lastModified
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text                        (Text)
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)

import           FileCollector.Common.Utils.Aeson (lensDefaultOptions)

data File = File
  { _file_name         :: Text
  , _file_uploaderName :: Text
  , _file_hash         :: Text
  , _file_lastModified :: UTCTime
  } deriving (Show, Generic)

makeLenses ''File

instance FromJSON File where
  parseJSON = genericParseJSON lensDefaultOptions

instance ToJSON File where
  toJSON = genericToJSON lensDefaultOptions
  toEncoding = genericToEncoding lensDefaultOptions
