{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module FileCollector.Common.Types.Password
  ( Password(..)
  ) where

import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant.API
import qualified Servant.Docs as Docs

newtype Password = Password Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

instance Docs.ToSample Password where
  toSamples _ = Docs.singleSample $ Password "awiefoaewjo123"
