{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.Config
  ( Config(..)
  , config
  , config_serverIp
  , config_serverPort
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.FileEmbed (embedFile)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import FileCollector.Common.Base.Aeson (lensDefaultOptions)

configBs :: ByteString
configBs =
#ifdef PRODUCTION
    $(embedFile "values/config/prod.json")
#else
    $(embedFile "values/config/dev.json")
#endif

data Config = Config
  { _config_serverIp   :: Text
  , _config_serverPort :: Int
  } deriving (Generic, Show, Eq)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = genericParseJSON lensDefaultOptions

config :: Maybe Config
config = decode (LBS.fromStrict configBs)
