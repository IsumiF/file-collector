module FileCollector.Backend.Oss.Class.MonadReadAliyunConfig
  ( MonadReadAliyunConfig(..)
  ) where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.Text (Text)

import FileCollector.Backend.App
import FileCollector.Backend.Config
import FileCollector.Backend.Oss.Impl.Aliyun.Internal

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
