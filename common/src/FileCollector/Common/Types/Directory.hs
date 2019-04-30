{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Common.Types.Directory
  ( Directory(..)
  , UploadRule(..)
  -- *Lens
  -- **Directory
  , directory_name
  , directory_ownerName
  , directory_expirationTime
  , directory_uploadRules
  -- **UploadRule
  , _RuleMaxFileSize
  , _RuleFileNameFormat
  , _RuleMaxFiles
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text                        (Text)
import           Data.Time                        (UTCTime)
import           Data.Word                        (Word32)
import           GHC.Generics

import           FileCollector.Common.Utils.Aeson (removeFieldNs)

data Directory = Directory
  { _directory_name           :: Text
  , _directory_ownerName      :: Text
  -- | Expiration time. @Nothing@ means the directory never expires.
  , _directory_expirationTime :: Maybe UTCTime
  , _directory_uploadRules    :: [UploadRule]
  } deriving (Generic, Show)

data UploadRule =
  -- | maximum file size, in bytes
    RuleMaxFileSize Word32
  -- | The regular expression that the file name must match
  | RuleFileNameFormat Text
  -- | Maximum number of files can be uploaded by a single uploader
  | RuleMaxFiles Int
    deriving (Generic, Show)

makeLenses ''Directory

makePrisms ''UploadRule

directoryJsonOptions :: Options
directoryJsonOptions = defaultOptions { fieldLabelModifier = removeFieldNs }

instance ToJSON Directory where
  toJSON = genericToJSON directoryJsonOptions
  toEncoding = genericToEncoding directoryJsonOptions

instance FromJSON Directory where
  parseJSON = genericParseJSON directoryJsonOptions

instance ToJSON UploadRule where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UploadRule
