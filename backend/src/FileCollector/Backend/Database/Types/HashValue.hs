{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module FileCollector.Backend.Database.Types.HashValue
  ( HashValue(..)
  ) where

import Data.Aeson
import Database.Persist.TH

import qualified FileCollector.Common.Types.HashValue as Common

newtype HashValue = HashValue Common.HashValue
  deriving (FromJSON, ToJSON, Eq, Show)

derivePersistFieldJSON "HashValue"
