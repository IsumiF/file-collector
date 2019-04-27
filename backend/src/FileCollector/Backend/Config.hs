{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Backend.Config
  ( Config
  , config_port
  , config_ip
  , config_dbConnStr
  , readConfigFromFile
  ) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { _config_port :: Int
  , _config_ip   :: Text
  , _config_dbConnStr :: Text
  } deriving (Generic, Show)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = removeFieldNs}

removeFieldNs :: String -> String
removeFieldNs = tail . dropWhile (/= '_') . tail

readConfigFromFile :: MonadIO m
                   => FilePath
                   -> m (Maybe Config)
readConfigFromFile filepath = liftIO $ decodeFileStrict' filepath
