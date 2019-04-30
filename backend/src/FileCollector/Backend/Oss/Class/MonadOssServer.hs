{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module FileCollector.Backend.Oss.Class.MonadOssServer
  ( MonadOssServer(..)
  , PackageFileError(..)
  , _PkgFileNotExist
  , _PkgCanNotUpload
  , _PkgSucceed
  ) where

import Control.Lens

import FileCollector.Common.Types.OssProvider

class (OssProvider provider, Monad m) => MonadOssServer provider m where
  getUploadCredential :: FileLocation provider -> m (Credential provider)
  getDownloadCredential :: FileLocation provider -> m (Credential provider)
  packageFiles :: [FileLocation provider]
               -> FileLocation provider
               -> m (PackageFileError (FileLocation provider) a)

data PackageFileError location a =
    PkgFileNotExist location
  | PkgCanNotUpload
  | PkgSucceed a

makePrisms ''PackageFileError
