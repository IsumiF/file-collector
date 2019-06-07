{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.Types.ServiceAccessors
  ( ServiceAccessors(..)
  , serviceAccessors_getDirList
  , serviceAccessors_getDir
  , serviceAccessors_putDir
  , serviceAccessors_deleteDir
  , serviceAccessors_getDirUploaders
  , serviceAccessors_putDirUploaders
  , serviceAccessors_getDirContent
  , serviceAccessors_getFile
  , serviceAccessors_putFile
  , serviceAccessors_deleteFile
  , serviceAccessors_commitPutFile
  , serviceAccessors_batchDownload
  , serviceAccessors_getUser
  ) where

import Control.Lens
import Data.Text (Text)
import Reflex
import Servant.API
import Servant.Reflex

import FileCollector.Common.Types

data ServiceAccessors t m = ServiceAccessors
  { _serviceAccessors_getDirList ::
         Dynamic t (Maybe BasicAuthData)
      -> Event t ()
      -> m (Event t (ReqResult () [Directory]))
  , _serviceAccessors_getDir ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Event t ()
      -> m (Event t (ReqResult () Directory))
  , _serviceAccessors_putDir ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Dynamic t (Either Text Directory)
      -> Event t ()
      -> m (Event t (ReqResult () ()))
  , _serviceAccessors_deleteDir ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Event t ()
      -> m (Event t (ReqResult () DirectoryDeleteResult))
  , _serviceAccessors_getDirUploaders ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Event t ()
      -> m (Event t (ReqResult () [UserName]))
  , _serviceAccessors_putDirUploaders ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Dynamic t (Either Text [UserName])
      -> Event t ()
      -> m (Event t (ReqResult () ()))
  , _serviceAccessors_getDirContent ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Event t ()
      -> m (Event t (ReqResult () [File]))
  , _serviceAccessors_getFile ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text FileName)
      -> Dynamic t Bool
      -> Event t ()
      -> m (Event t (ReqResult () (File, Maybe (OssClientCredential Aliyun))))
  , _serviceAccessors_putFile ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text FileName)
      -> Dynamic t (QParam FileName)
      -> Event t ()
      -> m (Event t (ReqResult () (OssClientCredential Aliyun)))
  , _serviceAccessors_deleteFile ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text FileName)
      -> Event t ()
      -> m (Event t (ReqResult () ()))
  , _serviceAccessors_commitPutFile ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text DirectoryName)
      -> Dynamic t (Either Text UserName)
      -> Dynamic t (Either Text FileName)
      -> Event t ()
      -> m (Event t (ReqResult () ()))
  , _serviceAccessors_batchDownload ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text [DownloadRequest])
      -> Event t ()
      -> m (Event t (ReqResult () [OssClientCredential Aliyun]))
  , _serviceAccessors_getUser ::
         Dynamic t (Maybe BasicAuthData)
      -> Dynamic t (Either Text UserName)
      -> Event t ()
      -> m (Event t (ReqResult () User))
  }

makeLenses ''ServiceAccessors

 -- Dynamic t (Maybe BasicAuthData) -> Event t () -> m (Event t (ReqResult () [Directory]))
-- Dynamic t (Maybe BasicAuthData) -> Dynamic t (Either Data.Text.Internal.Text UserName) -> Dynamic t (Either Data.Text.Internal.Text DirectoryName) -> Event t () -> m (Event t (ReqResult () Directory))
-- putDir :: Dynamic t (Maybe BasicAuthData) -> Dynamic t (Either Data.Text.Internal.Text UserName) -> Dynamic t (Either Data.Text.Internal.Text DirectoryName) -> Dynamic t (Either Data.Text.Internal.Text Directory) -> Event t () -> m (Event t (ReqResult () ()))
