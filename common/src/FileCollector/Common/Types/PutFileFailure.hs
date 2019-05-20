{-# LANGUAGE DeriveGeneric #-}

module FileCollector.Common.Types.PutFileFailure
  ( PutFileFailure(..)
  ) where

import           Data.Aeson
import           GHC.Generics (Generic)
import qualified Servant.Docs as Docs

data PutFileFailure = PutFileSystemTooBusy
                    | PutFileDirNotExist
                    | PutFilePermissionDenied
                      deriving (Show, Eq, Generic)

instance FromJSON PutFileFailure

instance ToJSON PutFileFailure where
  toEncoding = genericToEncoding defaultOptions

instance Docs.ToSample PutFileFailure where
  toSamples _ = Docs.singleSample PutFileSystemTooBusy
