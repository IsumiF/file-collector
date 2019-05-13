module FileCollector.Backend.Oss.Class.MonadReadAliyunConfig
  ( MonadReadAliyunConfig(..)
  ) where

import Data.Text (Text)
import Control.Monad.Reader (ask)
import Control.Lens

import FileCollector.Backend.Oss.Aliyun
import FileCollector.Backend.App
import FileCollector.Backend.Config

class Monad m => MonadReadAliyunConfig m where
  getAccessKey :: m AccessKey
  getObjectId :: Text -- ^object name
              -> m ObjectId

instance MonadReadAliyunConfig App where
  getAccessKey = do
    aliyunConfig <- askAliyunConfig
    pure $ AccessKey
      (aliyunConfig ^. configOssAliyun_accessKeyId)
      (aliyunConfig ^. configOssAliyun_accessKeySecret)
  getObjectId objectName = do
    aliyunConfig <- askAliyunConfig
    pure $ ObjectId
      (aliyunConfig ^. configOssAliyun_endPoint)
      (aliyunConfig ^. configOssAliyun_bucketName)
      objectName

askAliyunConfig :: App ConfigOssAliyun
askAliyunConfig = do
    appEnv <- ask
    pure $ appEnv ^. (appEnv_oss . configOss_aliyun)
