{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Common.Types.Credential
  ( Credential
  ) where

import           Control.Lens
import           Data.Text    (Text)

data Credential = Credential
  { _credential_username :: Text
  , _credential_password :: Text
  } deriving Show

makeLenses ''Credential
