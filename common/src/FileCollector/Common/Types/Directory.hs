{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module FileCollector.Common.Types.Directory
  ( Directory(..)
  , DirectoryName(..)
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
import           Data.Text (Text)
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Word (Word32)
import           GHC.Generics
import           Servant.API
import qualified Servant.Docs as Docs

import FileCollector.Common.Base.Aeson (removeFieldNs)
import FileCollector.Common.Types.User (UserName (..))

data Directory = Directory
  { _directory_name           :: DirectoryName
  , _directory_ownerName      :: UserName
  -- | Expiration time. @Nothing@ means the directory never expires.
  , _directory_expirationTime :: Maybe UTCTime
  , _directory_uploadRules    :: [UploadRule]
  } deriving (Generic, Show, Eq)

newtype DirectoryName = DirectoryName Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data UploadRule =
  -- | maximum file size, in bytes
    RuleMaxFileSize Word32
  -- | The regular expression that the file name must match
  | RuleFileNameFormat Text
  -- | Maximum number of files can be uploaded by a single uploader
  | RuleMaxFiles Int
    deriving (Generic, Show, Eq)

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

instance Docs.ToSample Directory where
  toSamples _ = Docs.singleSample $
    Directory
      (DirectoryName "Statistics")
      (UserName "TA-01")
      (Just $ UTCTime (fromGregorian 2019 04 23) 0)
      sampleRules

instance Docs.ToSample UploadRule where
  toSamples _ = Docs.samples sampleRules

sampleRules :: [UploadRule]
sampleRules =
    [ RuleMaxFileSize (80 * megaBytes)
    , RuleFileNameFormat "[[:digit:]]{8}-.+\\.[[:alnum:]\\.]+"
    , RuleMaxFiles 1
    ]

megaBytes :: Word32
megaBytes = 1024 * 1024
