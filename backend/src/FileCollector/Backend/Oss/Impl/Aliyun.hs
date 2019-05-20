module FileCollector.Backend.Oss.Impl.Aliyun
  ( Internal.initialize
  , Internal.deinitialize
  , Internal.ObjectId(..)
  , Internal.objectId_endPoint
  , Internal.objectId_bucketName
  , Internal.objectId_objectName
  , Internal.AccessKey(..)
  , Internal.accessKey_id
  , Internal.accessKey_secret
  , module FileCollector.Common.Types.OssProviderImpl.Aliyun
  , AliyunFileName(..)
  , AliyunError(..)
  , fromRawPath
  , getUploadCredential
  , getDownloadCredential
  , deleteFile
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           FileCollector.Backend.Oss.Class.MonadReadAliyunConfig
import qualified FileCollector.Backend.Oss.Impl.Aliyun.Internal as Internal
import qualified FileCollector.Common.Types as Common
import           FileCollector.Common.Types.OssProviderImpl.Aliyun

newtype AliyunFileName = AliyunFileName Text

data AliyunError = AliyunError

fromRawPath :: ByteString -> AliyunFileName
fromRawPath rawPath = AliyunFileName $ Text.decodeUtf8 rawPath

getUploadCredential :: (MonadReadAliyunConfig m, MonadIO m)
                    => AliyunFileName
                    -> m (Either AliyunError (Common.OssClientCredential Aliyun))
getUploadCredential (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    cred <- Common.AliyunSignedUrl <$> liftIO (Internal.getUploadUrl accessKey objId)
    pure $ Right cred

getDownloadCredential :: (MonadReadAliyunConfig m, MonadIO m)
                      => AliyunFileName
                      -> m (Either AliyunError (Common.OssClientCredential Aliyun))
getDownloadCredential (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    cred <- Common.AliyunSignedUrl <$> liftIO (Internal.getDownloadUrl accessKey objId)
    pure $ Right cred

deleteFile :: (MonadReadAliyunConfig m, MonadIO m)
           => AliyunFileName
           -> m Bool
deleteFile (AliyunFileName objectName) = do
    accessKey <- getAccessKey
    objId <- getObjectId objectName
    liftIO $ Internal.deleteFile accessKey objId
