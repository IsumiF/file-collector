{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module FileCollector.Common.Types.File
  ( File(..)
  , FileName(..)
  -- *Lens
  , file_name
  , file_uploaderName
  , file_hash
  , file_lastModified
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.String (IsString)
import           Data.Text (Text)
import           Data.Time (UTCTime (..), fromGregorian)
import           GHC.Generics (Generic)
import           Servant.API
import qualified Servant.Docs as Docs

import FileCollector.Common.Base.Aeson (lensDefaultOptions)
import FileCollector.Common.Types.HashValue
import FileCollector.Common.Types.User (UserName (..))

data File = File
  { _file_name         :: FileName
  , _file_uploaderName :: UserName
  , _file_hash         :: HashValue
  , _file_lastModified :: UTCTime
  } deriving (Generic, Show, Eq)

newtype FileName = FileName Text
  deriving (Generic, Show, Eq, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, IsString)

makeLenses ''File

instance FromJSON File where
  parseJSON = genericParseJSON lensDefaultOptions

instance ToJSON File where
  toJSON = genericToJSON lensDefaultOptions
  toEncoding = genericToEncoding lensDefaultOptions

instance Docs.ToSample File where
  toSamples _ = Docs.singleSample $ File
    (FileName "16337060-isumi.tar.gz")
    (UserName "Isumi Fly")
    (HashValue HashTypeMD5 "38b8c2c1093dd0fec383a9d9ac940515")
    (UTCTime (fromGregorian 2019 3 24) 0)
