{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module FileCollector.Backend.Core.File
  ( -- * Top level functions
    getVisibleDirectories
  , getDirectory
  , updateDirectory
  , UpdateDirectoryError(..)
  , deleteDirectory
  , DeleteDirectoryError(..)
  , putDirUploaders
  , getDirUploaders
  , getDirContent
  , getFile
  , putFile
  , deleteFile
  , commitPutFile
    -- * Inner functions
  , dirDbToCommon
  , dirCommonToDb
  ) where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Proxy (Proxy (..))
import           Data.String.Interpolate (i)
import           Data.Text (Text)
import           Data.Traversable (for)
import qualified Data.UUID as UUID

import           FileCollector.Backend.Core.File.Authorization
import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
import qualified FileCollector.Backend.Database.Class.MonadDirectoryContent as Db
import qualified FileCollector.Backend.Database.Class.MonadDirectoryUploader as Db
import qualified FileCollector.Backend.Database.Class.MonadPendingUploadFile as Db
import qualified FileCollector.Backend.Database.Class.MonadReadDirectory as Db
import qualified FileCollector.Backend.Database.Class.MonadReadFile as Db
import qualified FileCollector.Backend.Database.Class.MonadReadUser as Db
import qualified FileCollector.Backend.Database.Class.MonadWriteDirectory as Db
import qualified FileCollector.Backend.Database.Class.MonadWriteFile as Db
import qualified FileCollector.Backend.Database.Types as Db
import           FileCollector.Backend.IO.Time.Class.MonadCurrentTime
import           FileCollector.Backend.IO.UUID.Class.MonadUUID
import qualified FileCollector.Backend.Oss.Class.MonadOssService as Oss
import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types

getVisibleDirectories ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , MonadLogger m
  )
  => User
  -> m [Directory]
getVisibleDirectories me = do
    let role = me ^. user_role
    let name = me ^. user_name
    $(logInfo) [i|#{name} is reading list of visible directories|]
    Db.withConnection $
      case role of
        RoleUploader  -> getUploaderVisibleDirs name
        RoleCollector -> getCollectorOwnDirs name
        RoleAdmin     -> getAllDirectories

getUploaderVisibleDirs ::
  ( Db.MonadReadDirectory m
  , Db.MonadReadUser m
  , MonadLogger m
  )
  => UserName
  -> m [Directory]
getUploaderVisibleDirs (UserName name) = do
    dbDirs <- Db.getVisibleDirectories name
    traverse dirDbToCommon dbDirs

getCollectorOwnDirs ::
  ( Db.MonadReadDirectory m
  , Db.MonadReadUser m
  , MonadLogger m
  )
  => UserName
  -> m [Directory]
getCollectorOwnDirs (UserName name) = do
    dbDirs <- Db.getOwnDirectories name
    traverse dirDbToCommon dbDirs

getAllDirectories ::
  ( Db.MonadReadDirectory m
  , Db.MonadReadUser m
  , MonadLogger m
  )
  => m [Directory]
getAllDirectories = do
    dbDirs <- Db.getAllDirectories
    traverse dirDbToCommon dbDirs

getDirectory ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , MonadLogger m
  )
  => User -- ^ current user
  -> UserName -- ^ owner name
  -> DirectoryName -- ^ directory name
  -> m (Maybe Directory)
getDirectory user name dir = Db.withConnection (getDirectory' user name dir)

toText :: Convertible a Text => a -> Text
toText = convert

getDirectory' ::
  ( Db.MonadReadDirectory m
  , Db.MonadReadUser m
  , MonadLogger m
  )
  => User
  -> UserName
  -> DirectoryName
  -> m (Maybe Directory)
getDirectory' user ownerName dirName = do
    $(logInfo) [i|User #{toText (user ^. user_name)} is accessing directory #{toText ownerName}/#{toText dirName}|]
    case user ^. user_role of
      RoleAdmin -> getDir
      RoleCollector ->
        if user ^. user_name == convert ownerName
        then getDir
        else pure Nothing
      RoleUploader -> do
        maybeDirId <- Db.getDirectoryId (convert ownerName) (convert dirName)
        case maybeDirId of
          Nothing -> pure Nothing
          Just dirId -> do
            visible <- Db.isDirectoryVisible (convert $ user ^. user_name) dirId
            if visible then getDir else pure Nothing
  where
    getDir = do
      maybeDir <- Db.getDirectory (convert ownerName) (convert dirName)
      traverse dirDbToCommon maybeDir

dirDbToCommon ::
  ( Db.MonadReadDirectory m
  , Db.MonadReadUser m
  , MonadLogger m
  )
  => Db.Directory
  -> m Directory
dirDbToCommon (Db.Directory name ownerId expTime uploadRules) = do
    maybeOwner <- Db.getUserById ownerId
    ownerName <- case maybeOwner of
      Nothing -> do
        $(logError) [i|Directory #{name} has a non-exist owner #{ownerId}|]
        pure ""
      Just owner -> pure $ Db.userName owner
    let uploadRules' = fmap convert uploadRules :: [UploadRule]
    pure $ Directory
      (DirectoryName name)
      (UserName ownerName)
      expTime
      uploadRules'

dirCommonToDb ::
  ( Db.MonadReadUser m
  , Db.MonadWriteDirectory m
  )
  => Directory
  -> m (Maybe Db.Directory)
dirCommonToDb dir = do
    let ownerName = dir ^. directory_ownerName
    maybeOwnerId <- Db.getIdByUserName (convert ownerName)
    case maybeOwnerId of
      Nothing -> pure Nothing
      Just ownerId -> do
        let result = Db.Directory
              (convert (dir ^. directory_name))
              ownerId
              (dir ^. directory_expirationTime)
              (fmap convert (dir ^. directory_uploadRules))
        pure $ Just result

updateDirectory ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadWriteDirectory (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , MonadLogger m
  )
  => User -- ^ current user
  -> UserName -- ^ directory owner's name
  -> DirectoryName -- ^ directory name
  -> Directory -- ^ new directory
  -> m (Either UpdateDirectoryError ())
updateDirectory me ownerName dirName newDir = Db.withConnection $ do
    let role = me ^. user_role
    if role == RoleAdmin || role == RoleCollector && me ^. user_name == ownerName
    then runExceptT $ do
      maybeToExceptT UDErrCanNotUpdate $ do
        newOwner <- MaybeT $ Db.getUserByName (convert $ newDir ^. directory_ownerName)
        if Db.userRole newOwner == convert RoleCollector
        then pure ()
        else MaybeT $ pure Nothing
      maybeToExceptT UDErrNoSuchDirectory $ do
        dirId <- MaybeT $ Db.getDirectoryId (convert ownerName) (convert dirName)
        newDirDb <- MaybeT $ dirCommonToDb newDir
        lift $ Db.updateDirectory dirId newDirDb
    else
      pure $ Left UDErrNoSuchDirectory

data UpdateDirectoryError = UDErrNoSuchDirectory
                          | UDErrCanNotUpdate

deleteDirectory ::
  forall provider m backend.
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadWriteDirectory (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , Db.MonadDirectoryContent (ReaderT backend m)
  , Db.MonadWriteFile (ReaderT backend m)
  , Db.MonadDirectoryUploader (ReaderT backend m)
  , Db.MonadPendingUploadFile (ReaderT backend m)
  , Oss.MonadOssService provider m
  )
  => Proxy provider
  -> User -- ^ current user
  -> UserName -- ^ directory owner
  -> DirectoryName -- ^ directory name
  -> m (Either DeleteDirectoryError ())
deleteDirectory _ me ownerName dirName =
    Db.withConnection $ runExceptT $ do
      if not $
        me ^. user_name == ownerName && me ^. user_role == RoleCollector
        || me ^. user_role == RoleAdmin
      then throwE DdeNoSuchDirectory
      else pure ()
      dirId <- maybeToExceptT DdeNoSuchDirectory $ MaybeT $ Db.getDirectoryId (convert ownerName) (convert dirName)
      files <- lift $ Db.getDirectoryContent dirId

      deleteStatList <- for files $ \(fileId, file) -> do
        let path = Oss.fromRawPath (Db.fileRawPath file) :: Oss.FileName provider
        deleteStatus <- Oss.deleteFile path
        if deleteStatus
        then lift $ Db.deleteFile fileId
        else pure ()
        pure deleteStatus
      if and deleteStatList
      then do
        Db.deleteAllUploadersOfDir dirId
        Db.removeAllPendingUploadFilesOfDirectory dirId
        Db.deleteDirectory dirId
      else throwE DdePartiallyDeleted

data DeleteDirectoryError = DdeNoSuchDirectory
                          | DdePartiallyDeleted

putDirUploaders ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadUser (ReaderT backend m)
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadDirectoryUploader (ReaderT backend m)
  )
  => User -- ^me
  -> UserName
  -> DirectoryName
  -> [UserName]
  -> m Bool
putDirUploaders me ownerName dirName newUploaders =
    Db.withConnection $ rollBackOnError $ fmap isJust $ runMaybeT $
      if (me ^. user_name == ownerName) || (me ^. user_role) == RoleAdmin
      then do
        dirId <- MaybeT $ Db.getDirectoryId (convert ownerName) (convert dirName)
        lift $ Db.deleteAllUploadersOfDir dirId
        for newUploaders $ \newUploaderName -> do
          newUploaderId <- MaybeT $ Db.getIdByUserName (convert newUploaderName)
          lift $ Db.addUploaderToDir dirId newUploaderId
      else MaybeT $ pure Nothing

rollBackOnError ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  )
  => ReaderT backend m Bool
  -> ReaderT backend m Bool
rollBackOnError action = do
    ret <- action
    if not ret
    then Db.transactionUndo
    else pure ()
    pure ret

getDirUploaders ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadDirectoryUploader (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  )
  => User -- ^me
  -> UserName
  -> DirectoryName
  -> m (Maybe [UserName])
getDirUploaders me ownerName dirName =
    Db.withConnection $ runMaybeT $
      if (me ^. user_name == ownerName) || (me ^. user_role) == RoleAdmin
      then do
        dirId <- MaybeT $ Db.getDirectoryId (convert ownerName) (convert dirName)
        userIds <- lift $ Db.getDirUploaders dirId
        for userIds $ \userId -> do
          name <- fmap Db.userName (MaybeT $ Db.getUserById userId)
          pure (UserName name)
      else MaybeT $ pure Nothing

extractMaybeList :: Maybe [a] -> [a]
extractMaybeList Nothing   = []
extractMaybeList (Just xs) = xs

getDirContent ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadUser (ReaderT backend m)
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadDirectoryUploader (ReaderT backend m)
  , Db.MonadDirectoryContent (ReaderT backend m)
  )
  => User
  -> UserName
  -> DirectoryName
  -> m [File]
getDirContent me ownerName dirName =
    Db.withConnection $ fmap extractMaybeList $ runMaybeT $ do
      dirId <- MaybeT $ Db.getDirectoryId (convert ownerName) (convert dirName)
      meId <- MaybeT $ Db.getIdByUserName (convert $ me ^. user_name)
      es <- case me ^. user_role of
        RoleUploader -> do
          dirHasUploader <- Db.dirHasUploader dirId meId
          maybeTJustIf dirHasUploader $
            Db.getDirectoryContentOfUploader dirId meId
        RoleCollector ->
          maybeTJustIf (me ^. user_name == ownerName) $
            Db.getDirectoryContent dirId
        RoleAdmin -> Db.getDirectoryContent dirId
      traverse (MaybeT . fileDbToCommon . snd)  es

fileDbToCommon ::
  Db.MonadReadUser m
  => Db.File
  -> m (Maybe File)
fileDbToCommon dbFile = runMaybeT $ do
    let uploaderId = Db.fileUploader dbFile
    uploader <- MaybeT $ Db.getUserById uploaderId
    let uploaderName = Db.userName uploader
    pure $ File
      (convert $ Db.fileName dbFile)
      (convert uploaderName)
      (convert $ Db.fileHashValue dbFile)
      (Db.fileLastModified dbFile)

maybeTJustIf :: Monad m
             => Bool
             -> m a
             -> MaybeT m a
maybeTJustIf p action = maybeTJustIf' p (lift action)

maybeTJustIf' :: Monad m
              => Bool
              -> MaybeT m a
              -> MaybeT m a
maybeTJustIf' p action = if p then action else MaybeT (pure Nothing)

getFile ::
  forall m backend ossProvider.
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadFile (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , Db.MonadReadDirectory (ReaderT backend m)
  , Oss.MonadOssService ossProvider m
  , Db.MonadDirectoryUploader (ReaderT backend m)
  )
  => User -- ^me
  -> FullFilePath
  -> Bool -- ^whether to return credential
  -> m (Maybe (File, Maybe (OssClientCredential ossProvider)))
getFile me fullFilePath returnCred =
    Db.withConnection $ fmap join $ runMaybeT $ runMaybeT $
      withFileAuthorized me fullFilePath $ \_ _ _ fileId -> do
        fileId' <- MaybeT $ pure fileId
        dbFile <- MaybeT $ Db.getFileById fileId'
        file <- MaybeT $ fileDbToCommon dbFile
        maybeCred <-
          if returnCred
          then runMaybeT $ do
            let ossFileName = Oss.fromRawPath (Db.fileRawPath dbFile)
            exceptToMaybeT . ExceptT $ Oss.getDownloadCredential ossFileName
          else pure Nothing
        pure (file, maybeCred)

{-| 上传文件。返回上传证书，把文件加入PendingUploadFile表

该函数包含多种功能：
- 创建文件。如果使用给定文件名找不到文件，则忽略新文件名，直接按照原文件名创建文件
- 覆盖已有文件。使用给定文件名能够找到文件，并且新文件名为@Nothing@或者和原文件名相同，则覆盖该文件
- 重命名文件。使用给定文件名能找到文件，并且新文件名不同，则覆盖并重命名文件
-}
putFile ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadReadFile (ReaderT backend m)
  , Db.MonadDirectoryUploader (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , Db.MonadPendingUploadFile (ReaderT backend m)
  , Oss.MonadOssService ossProvider m
  , MonadCurrentTime m
  , MonadUUID m
  )
  => User -- ^current user
  -> FullFilePath
  -> Maybe FileName -- ^new file name
  -> m (Either PutFileFailure (OssClientCredential ossProvider))
putFile me fullFilePath newFileName =
    Db.withConnection $ fmap join $ runExceptT $ runExceptT $ maybeToExceptT PutFilePermissionDenied $
      withFileAuthorized me fullFilePath $ \_ uploaderId dirId fileId -> do
        file <- join <$> traverse Db.getFileById fileId
        let rawPath = fmap Db.fileRawPath file
        maybeToExceptT PutFileSystemTooBusy $ MaybeT $
          putPendingUploadFile (fullFilePath ^. fullFilePath_fileName) uploaderId dirId newFileName rawPath

putPendingUploadFile ::
  ( Db.MonadPendingUploadFile m
  , MonadCurrentTime m
  , Oss.MonadOssService oss m
  , MonadUUID m
  )
  => FileName
  -> Db.UserId
  -> Db.DirectoryId
  -> Maybe FileName
  -> Maybe ByteString -- ^existing raw path in database
  -> m (Maybe (OssClientCredential oss))
putPendingUploadFile fileName uploaderId dirId newFileName rawPath = runMaybeT $ do
    let pendingKey = Db.UniquePendingUploadFile (convert fileName) uploaderId dirId
    pendingEntry <- Db.getPendingUploadFile pendingKey
    rawPath' <-
      case pendingEntry of
        Just pendingEntry' -> do
          updateExistingPendingFile fileName uploaderId dirId newFileName
          pure $ Db.pendingUploadFileRawPath pendingEntry'
        Nothing -> do
          uuid <- MaybeT nextUUID
          let rawPath' = fromMaybe (UUID.toASCIIBytes uuid) rawPath
          addNewPendingFile fileName uploaderId dirId newFileName rawPath'
          pure rawPath'
    exceptToMaybeT $ ExceptT $ Oss.getUploadCredential (Oss.fromRawPath rawPath')

addNewPendingFile ::
  ( Db.MonadPendingUploadFile m
  , MonadCurrentTime m
  )
  => FileName -- ^file name
  -> Db.UserId -- ^uploader
  -> Db.DirectoryId -- ^the containing directory
  -> Maybe FileName -- ^new file name
  -> ByteString
  -> m ()
addNewPendingFile fileName uploaderId dirId newFileName rawPath = do
    now <- currentTime
    Db.addPendingUploadFile $ Db.PendingUploadFile
      (convert fileName) (fmap convert newFileName) uploaderId dirId now rawPath

updateExistingPendingFile ::
  ( Db.MonadPendingUploadFile m
  , MonadCurrentTime m
  )
  => FileName -- ^file name
  -> Db.UserId -- ^uploader id
  -> Db.DirectoryId -- ^directory id
  -> Maybe FileName -- ^new name
  -> m ()
updateExistingPendingFile (FileName fileName) uploaderId dirId newFileName = do
    now <- currentTime
    Db.updatePendingUploadFile
      (Db.UniquePendingUploadFile fileName uploaderId dirId)
      (fmap convert newFileName)
      now

deleteFile ::
  forall oss backend m. ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Oss.MonadOssService oss m
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadReadFile (ReaderT backend m)
  , Db.MonadWriteFile (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , Db.MonadDirectoryUploader (ReaderT backend m)
  )
  => Proxy oss
  -> User -- ^me
  -> FullFilePath
  -> m (Maybe ())
deleteFile _ me fullFilePath =
    Db.withConnection $ fmap join $ runMaybeT $ runMaybeT $
      withFileAuthorized me fullFilePath $ \_ _ _ fileId -> do
        fileId' <- MaybeT $ pure fileId
        dbFile <- MaybeT $ Db.getFileById fileId'
        let ossFileName :: Oss.FileName oss = Oss.fromRawPath $ Db.fileRawPath dbFile
        deleteSuccess <- Oss.deleteFile ossFileName
        if deleteSuccess then Db.deleteFile fileId' else pure ()

commitPutFile ::
  forall m backend oss.
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadWriteFile (ReaderT backend m)
  , Db.MonadReadDirectory (ReaderT backend m)
  , Db.MonadReadUser (ReaderT backend m)
  , Db.MonadDirectoryUploader (ReaderT backend m)
  , Db.MonadPendingUploadFile (ReaderT backend m)
  , Oss.MonadOssService oss m
  )
  => Proxy oss
  -> User
  -> FullFilePath
  -> m (Maybe ())
commitPutFile _ me fullFilePath =
    Db.withConnection $ fmap join $ runMaybeT $ runMaybeT $
      withFileAuthorized me fullFilePath $ \_ uploaderId dirId fileId -> do
        let pendingEntryKey = Db.UniquePendingUploadFile
              (convert $ fullFilePath ^. fullFilePath_fileName) uploaderId dirId
        pendingEntry <- MaybeT $ Db.getPendingUploadFile pendingEntryKey
        let ossFileName :: Oss.FileName oss =
              Oss.fromRawPath $ Db.pendingUploadFileRawPath pendingEntry
        fileMetaInfo <- MaybeT $ Oss.getFileMetaInfo ossFileName
        let lastModified = fileMetaInfo ^. Oss.fileMetaInfo_lastModified
            lastModified' = max
              (Db.pendingUploadFileRequestTime pendingEntry) lastModified
            mkNewFile = Db.File
              (Db.pendingUploadFileName pendingEntry)
              (convert $ HashValue HashTypeMD5 "")
              (Db.pendingUploadFileUploader pendingEntry)
              (Db.pendingUploadFileDirectory pendingEntry)
              lastModified'
        case fileId of
          Nothing ->
            void $ Db.addFile $ mkNewFile (Db.pendingUploadFileRawPath pendingEntry)
          Just fileId' -> do
            existingFile <- MaybeT $ Db.getFileById fileId'
            Db.updateFile fileId' (mkNewFile (Db.fileRawPath existingFile))
            Db.removePendingUploadFile pendingEntryKey
