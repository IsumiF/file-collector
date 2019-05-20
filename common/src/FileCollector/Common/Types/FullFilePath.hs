{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Common.Types.FullFilePath
  ( FullFilePath(..)
  , fullFilePath_dirOwnerName
  , fullFilePath_dirName
  , fullFilePath_uploaderName
  , fullFilePath_fileName
  ) where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics (Generic)
import qualified Servant.Docs as Docs

import FileCollector.Common.Base.Aeson (lensDefaultOptions)
import FileCollector.Common.Types.Directory
import FileCollector.Common.Types.File
import FileCollector.Common.Types.User

data FullFilePath = FullFilePath
  { _fullFilePath_dirOwnerName :: UserName
  , _fullFilePath_dirName      :: DirectoryName
  , _fullFilePath_uploaderName :: UserName
  , _fullFilePath_fileName     :: FileName
  } deriving (Show, Eq, Generic)

makeLenses ''FullFilePath

instance FromJSON FullFilePath where
  parseJSON = genericParseJSON lensDefaultOptions

instance ToJSON FullFilePath where
  toJSON = genericToJSON lensDefaultOptions
  toEncoding = genericToEncoding lensDefaultOptions

instance Docs.ToSample FullFilePath where
  toSamples _ = Docs.singleSample $
    FullFilePath
      (UserName "isumi")
      (DirectoryName "dsp-hw01")
      (UserName "zelinf")
      (FileName "my-homework-1.zip")
