{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Common.Types.DirectoryDeleteResult
  ( DirectoryDeleteResult(..)
  , _DdrFullyDeleted
  , _DdrPartiallyDeleted
  ) where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics (Generic)
import qualified Servant.Docs as Docs

data DirectoryDeleteResult = DdrFullyDeleted | DdrPartiallyDeleted
  deriving (Generic, Show, Eq)

makePrisms ''DirectoryDeleteResult

instance FromJSON DirectoryDeleteResult 

instance ToJSON DirectoryDeleteResult where
  toEncoding = genericToEncoding defaultOptions

instance Docs.ToSample DirectoryDeleteResult where
  toSamples _ = Docs.samples [DdrFullyDeleted, DdrPartiallyDeleted]
