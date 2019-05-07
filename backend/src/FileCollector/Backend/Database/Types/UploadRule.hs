{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module FileCollector.Backend.Database.Types.UploadRule
  ( UploadRule(..)
  ) where

import qualified Data.Aeson as Aeson
import           Database.Persist.TH (derivePersistFieldJSON)
import qualified FileCollector.Common.Types.Directory as Common
    (UploadRule (..))

newtype UploadRule = UploadRule Common.UploadRule
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq)

instance Show UploadRule where
  show (UploadRule x) = show x

derivePersistFieldJSON "UploadRule"
