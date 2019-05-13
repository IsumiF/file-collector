{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-|

File related APIs. Endpoints begin with @/dir@
-}
module FileCollector.Common.Api.File
  ( -- * Combined API
    Api
    -- * Inner APIs
    -- ** Directory
  , ApiDir
  , ApiGetDirList
  , ApiGetDir
  , ApiPutDir
  , ApiDeleteDir
  , ApiDirUploaders
  , ApiGetDirContent
    -- ** File
  , ApiGetFile
  , ApiPutFile
  , ApiDeleteFile
    -- **Batch download
  , ApiBatchDownload
  ) where

import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text)
import           Servant.API
import qualified Servant.Docs                               as Docs

import           FileCollector.Common.Api.Auth              (AuthCollector,
                                                             AuthUploader)
import           FileCollector.Common.Types.Directory
import           FileCollector.Common.Types.DownloadRequest
import           FileCollector.Common.Types.File
import           FileCollector.Common.Types.OssProvider
import           FileCollector.Common.Types.User

-- | Get all directories visible to current user
type Api ossProvider = "filesystem" :>
  ( ApiDir ossProvider
  :<|> ApiBatchDownload ossProvider
  )

type ApiDir ossProvider = "dir" :>
  ( ApiGetDirList
  :<|> ApiGetDir
  :<|> ApiPutDir
  :<|> ApiDeleteDir
  :<|> ApiDirUploaders
  :<|> ApiGetDirContent
  :<|> ApiGetFile ossProvider
  :<|> ApiPutFile ossProvider
  :<|> ApiDeleteFile
  )

-- | Get a list of directories
type ApiGetDirList = AuthUploader :> Get '[JSON] [Directory]

type ApiGetDir =
  AuthUploader
  :> CaptureOwnerAndDir (Get '[JSON] Directory)

type ApiPutDir =
  AuthCollector
  :> CaptureOwnerAndDir
    ( ReqBody '[JSON] Directory
    :> Put '[JSON] ()
    )

type ApiDeleteDir =
  AuthCollector
  :> CaptureOwnerAndDir (Delete '[JSON] ())

type ApiDirUploaders =
  AuthCollector
  :> CaptureOwnerAndDir
    ( "uploaders"
    :> ( ReqBody '[JSON] [UserName] :> Put '[JSON] ()
       :<|> Get '[JSON] [UserName]
       )
    )

type ApiGetDirContent =
  AuthUploader
  :> CaptureOwnerAndDir
    ( "file"
    :> Get '[JSON] [File]
    )

type ApiGetFile ossProvider =
  AuthUploader
  :> CaptureUploaderAndFile
    ( QueryFlag "content"
    :> Get '[JSON] (File, Maybe (OssClientCredential ossProvider))
    )

type ApiPutFile ossProvider =
  AuthUploader
  :> CaptureUploaderAndFile
    ( QueryParam "newName" FileName
    :> Put '[JSON] (OssClientCredential ossProvider)
    )

type ApiDeleteFile =
  AuthUploader
  :> CaptureUploaderAndFile (Delete '[JSON] ())

type CaptureOwnerAndDir api =
  Capture "ownerName" UserName
  :> Capture "dirName" DirectoryName
  :> api

type CaptureUploaderAndFile api =
  CaptureOwnerAndDir
    ( "file"
    :> Capture "uploaderName" UserName
    :> Capture "fileName" FileName
    :> api
    )

type ApiBatchDownload ossProvider =
  AuthCollector
  :> "batchDownload"
  :> ReqBody '[JSON] [DownloadRequest]
  :> Get '[JSON] [OssClientCredential ossProvider]

instance Docs.ToCapture (Capture "ownerName" UserName) where
  toCapture _ =
    Docs.DocCapture "ownerName" "Name of directory owner"

instance Docs.ToCapture (Capture "dirName" DirectoryName) where
  toCapture _ =
    Docs.DocCapture "dirName" "Name of directory"

instance Docs.ToCapture (Capture "uploaderName" UserName) where
  toCapture _ =
    Docs.DocCapture "uploaderName" "Name of uploader"

instance Docs.ToCapture (Capture "fileName" FileName) where
  toCapture _ =
    Docs.DocCapture "fileName" "Name of file"

instance Docs.ToParam (QueryParam "newName" FileName) where
  toParam _ = Docs.DocQueryParam
    "newName"
    []
    "New name of the file"
    Docs.Normal

instance Docs.ToParam (QueryFlag "content") where
  toParam _ = Docs.DocQueryParam
    "content"
    []
    "Whether to fetch content of the file"
    Docs.Flag
