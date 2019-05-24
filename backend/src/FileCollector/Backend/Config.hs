{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Backend.Config
  ( Config(..)
  , readConfigFromFile
  , WarpConfig(..)
  , OtherConfig(..)
  , LogLevelWrapped(..)
  , ConfigOss(..)
  , ConfigOssAliyun(..)
  -- *Lens
  , config_warp
  , warpConfig_port
  , warpConfig_ip
  , config_other
  , otherConfig_dbConnStr
  , otherConfig_logLevel
  , otherConfig_oss
  , configOss_aliyun
  , configOssAliyun_accessKeyId
  , configOssAliyun_accessKeySecret
  , configOssAliyun_endPoint
  , configOssAliyun_bucketName
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
  { _config_warp :: WarpConfig
  , _config_other :: OtherConfig
  } deriving (Generic, Show)

data WarpConfig = WarpConfig
  { -- | Port
    _warpConfig_port :: Int
    -- | Server IPv4 address
  , _warpConfig_ip   :: Text
  } deriving (Generic, Show)

data OtherConfig = OtherConfig
  { _otherConfig_dbConnStr :: Text
  , _otherConfig_logLevel :: LogLevelWrapped
  , _otherConfig_oss :: ConfigOss
  } deriving (Generic, Show)

-- Simple wrapper of 'LogLevel' supporting 'FromJSON'
newtype LogLevelWrapped = LogLevelWrapped LogLevel

instance Show LogLevelWrapped where
  show (LogLevelWrapped x) = show x

newtype ConfigOss = ConfigOss
  { _configOss_aliyun :: ConfigOssAliyun
  } deriving (Generic, Show)

data ConfigOssAliyun = ConfigOssAliyun
  { _configOssAliyun_accessKeyId     :: Text
  , _configOssAliyun_accessKeySecret :: Text
  , _configOssAliyun_endPoint        :: Text
  , _configOssAliyun_bucketName      :: Text
  } deriving (Generic, Show)

makeLenses ''Config
makeLenses ''ConfigOss
makeLenses ''ConfigOssAliyun
makeLenses ''WarpConfig
makeLenses ''OtherConfig

instance FromJSON WarpConfig where
  parseJSON = genericParseJSON lensDefaultOptions

instance FromJSON OtherConfig where
  parseJSON = genericParseJSON lensDefaultOptions

instance FromJSON LogLevelWrapped where
  parseJSON = withText "expected string" $ \txt ->
    pure . LogLevelWrapped $
      case txt of
        "Debug" -> LevelDebug
        "Info"  -> LevelInfo
        "Warn"  -> LevelWarn
        "Error" -> LevelError
        _       -> LevelOther txt

instance FromJSON ConfigOss where
  parseJSON = genericParseJSON lensDefaultOptions

instance FromJSON ConfigOssAliyun where
  parseJSON = genericParseJSON lensDefaultOptions

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
-- >    "logLevel": "Debug",
-- >    "oss": {
-- >      "aliyun": {
-- >        "accessKeyId": "****",
-- >        "accessKeySecret": "****",
-- >        "endPoint": "oss-cn-shenzhen.aliyuncs.com",
-- >        "bucketName": "file-collector-test"
-- >      }
-- >    }
-- >  }
-- For @logLevel@, value @Debug@, @Info@, @Warn@ and @Error@ are mapped into
-- 'LevelDebug', 'LevelInfo', 'LevelWarn' and 'LevelError', respectively. Other
-- level values are converted to 'LevelOther' @levelName@
readConfigFromFile :: MonadIO m
                   => FilePath
                   -> m (Maybe Config)
readConfigFromFile filepath = liftIO $ decodeFileStrict' filepath
