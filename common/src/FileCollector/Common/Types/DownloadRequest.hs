{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Common.Types.DownloadRequest
  ( DownloadRequest(..)
  -- *Lens
  , downloadRequest_dirOwner
  , downloadRequest_dirName
  , downloadRequest_uploaderName
  , downloadRequest_fileName
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import qualified Servant.Docs                         as Docs

import           FileCollector.Common.Types.Directory (DirectoryName (..))
import           FileCollector.Common.Types.File      (FileName (..))
import           FileCollector.Common.Types.User      (UserName (..))
import           FileCollector.Common.Base.Aeson     (lensDefaultOptions)

data DownloadRequest = DownloadRequest
  { _downloadRequest_dirOwner     :: UserName
  , _downloadRequest_dirName      :: DirectoryName
  , _downloadRequest_uploaderName :: UserName
  , _downloadRequest_fileName     :: FileName
  } deriving Generic

makeLenses ''DownloadRequest

instance FromJSON DownloadRequest where
  parseJSON = genericParseJSON lensDefaultOptions

instance ToJSON DownloadRequest where
  toJSON = genericToJSON lensDefaultOptions
  toEncoding = genericToEncoding lensDefaultOptions

instance Docs.ToSample DownloadRequest where
  toSamples _ = Docs.singleSample $
    DownloadRequest
      (UserName "TA-01")
      (DirectoryName "dsp19")
      (UserName "isumi")
      (FileName "")
