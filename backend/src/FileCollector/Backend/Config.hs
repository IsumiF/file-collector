{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Backend.Config
  ( Config(..)
  , readConfigFromFile
  , LogLevelWrapped(..)
  -- *Lens
  , config_port
  , config_ip
  , config_dbConnStr
  , config_logLevel
  ) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LogLevel (..))
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import FileCollector.Common.Base.Aeson (lensDefaultOptions)

-- |Server initial configuration
data Config = Config
  { -- | Port
    _config_port      :: Int
    -- | Server IPv4 address
  , _config_ip        :: Text
    -- | Database connection string (Current database is Sqlite3)
  , _config_dbConnStr :: Text
    -- | Log level
  , _config_logLevel  :: LogLevelWrapped
  } deriving (Generic, Show)

-- Simple wrapper of 'LogLevel' supporting 'FromJSON'
newtype LogLevelWrapped = LogLevelWrapped LogLevel

instance Show LogLevelWrapped where
  show (LogLevelWrapped x) = show x

instance FromJSON LogLevelWrapped where
  parseJSON = withText "expected string" $ \txt ->
    pure . LogLevelWrapped $
      case txt of
        "Debug" -> LevelDebug
        "Info"  -> LevelInfo
        "Warn"  -> LevelWarn
        "Error" -> LevelError
        _       -> LevelOther txt

makeLenses ''Config

instance FromJSON Config where
  parseJSON = genericParseJSON lensDefaultOptions

-- |Read configuration from file. Returns Nothing if any error occur.
--
-- Configuration file is in JSON format. Example config:
--
-- >  {
-- >    "port": 8080,
-- >    "ip": "127.0.0.1",
-- >    "dbConnStr": "devres/data.db",
-- >    "logLevel": "Debug"
-- >  }
-- For @logLevel@, value @Debug@, @Info@, @Warn@ and @Error@ are mapped into
-- 'LevelDebug', 'LevelInfo', 'LevelWarn' and 'LevelError', respectively. Other
-- level values are converted to 'LevelOther' @levelName@
readConfigFromFile :: MonadIO m
                   => FilePath
                   -> m (Maybe Config)
readConfigFromFile filepath = liftIO $ decodeFileStrict' filepath
