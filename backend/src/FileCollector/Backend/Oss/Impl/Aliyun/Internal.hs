{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}

module FileCollector.Backend.Oss.Impl.Aliyun.Internal
  ( initialize
  , deinitialize
  , getUploadUrl
  , getDownloadUrl
  , deleteFile
  , getFileMetaInfo
  , ObjectId(..)
  , objectId_endPoint
  , objectId_bucketName
  , objectId_objectName
  , AccessKey(..)
  , accessKey_id
  , accessKey_secret
  , FileMetaInfo(..)
  , fileMetaInfo_lastModified
  ) where

import           Control.Lens
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime)
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.Ptr
import           Network.HTTP.Date (parseHTTPDate, httpDateToUTC)

foreign import ccall "fc_aos_initialize"
    cAosInitialize :: IO CBool

foreign import ccall "fc_aos_deinitialize"
    cAosDeinitialize :: IO ()

foreign import ccall "fc_aos_getUploadUrl"
    cAosGetUploadUrl :: CString
                     -> CString
                     -> CString
                     -> CString
                     -> CString
                     -> IO CString

foreign import ccall "fc_aos_getDownloadUrl"
    cAosGetDownloadUrl :: CString
                       -> CString
                       -> CString
                       -> CString
                       -> CString
                       -> IO CString

foreign import ccall "fc_aos_deleteFile"
    cAosDeleteFile :: CString
                   -> CString
                   -> CString
                   -> CString
                   -> CString
                   -> IO CBool

foreign import ccall "fc_aos_getFileMeta"
    cAosGetFileMeta :: CString
                    -> CString
                    -> CString
                    -> CString
                    -> CString
                    -> IO CString

foreign import ccall "free"
    cFree :: Ptr a -> IO ()

data ObjectId = ObjectId
  { _objectId_endPoint   :: Text
  , _objectId_bucketName :: Text
  , _objectId_objectName :: Text
  } deriving (Show, Eq)

makeLenses ''ObjectId

data AccessKey = AccessKey
  { _accessKey_id     :: Text
  , _accessKey_secret :: Text
  } deriving (Show, Eq)

makeLenses ''AccessKey

newtype FileMetaInfo = FileMetaInfo
  { _fileMetaInfo_lastModified :: UTCTime
  } deriving (Show, Eq)

makeLenses ''FileMetaInfo

initialize :: IO Bool
initialize = fromCBool <$> cAosInitialize

deinitialize :: IO ()
deinitialize = cAosDeinitialize

getUploadUrl :: AccessKey
             -> ObjectId
             -> IO Text
getUploadUrl accessKey objId =
    withObjectParams accessKey objId $ \cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName -> do
      cUrl <- cAosGetUploadUrl cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName
      bsUrl <- BS.packCString cUrl
      cFree cUrl
      pure $ T.decodeUtf8 bsUrl

getDownloadUrl :: AccessKey
               -> ObjectId
               -> IO Text
getDownloadUrl accessKey objId =
    withObjectParams accessKey objId $ \cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName -> do
      cUrl <- cAosGetDownloadUrl cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName
      bsUrl <- BS.packCString cUrl
      cFree cUrl
      pure $ T.decodeUtf8 bsUrl

deleteFile :: AccessKey
           -> ObjectId
           -> IO Bool
deleteFile accessKey objId =
    withObjectParams accessKey objId $ \cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName -> do
      deleteSucceeded <- cAosDeleteFile cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName
      pure $ fromCBool deleteSucceeded

getFileMetaInfo :: AccessKey
                -> ObjectId
                -> IO (Maybe FileMetaInfo)
getFileMetaInfo accessKey objId =
    withObjectParams accessKey objId $ \cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName -> do
      cLastModifiedStr <- cAosGetFileMeta cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName
      bsLastModified <- BS.packCString cLastModifiedStr
      cFree cLastModifiedStr
      pure $ FileMetaInfo . httpDateToUTC <$> parseHTTPDate bsLastModified

textWithCString :: Text -> (CString -> IO a) -> IO a
textWithCString str = BS.useAsCString (T.encodeUtf8 str)

withObjectParams ::
     AccessKey
  -> ObjectId
  -> (CString -> CString -> CString -> CString -> CString -> IO a)
  -> IO a
withObjectParams accessKey objId f =
    textWithCString (accessKey ^. accessKey_id) $ \cAccessKeyId ->
      textWithCString (accessKey ^. accessKey_secret) $ \cAccessKeySecret ->
        textWithCString (objId ^. objectId_endPoint) $ \cEndPoint ->
          textWithCString (objId ^. objectId_bucketName) $ \cBucketName ->
            textWithCString (objId ^. objectId_objectName) $ \cObjectName ->
              f cAccessKeyId cAccessKeySecret cEndPoint cBucketName cObjectName

fromCBool :: CBool -> Bool
fromCBool (CBool x) = x /= 0
