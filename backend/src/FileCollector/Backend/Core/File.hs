{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module FileCollector.Backend.Core.File
  ( getVisibleDirectories
  , getDirectory
  ) where

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate (i)

import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
import qualified FileCollector.Backend.Database.Class.MonadReadDirectory as Db
import qualified FileCollector.Backend.Database.Class.MonadReadUser as Db
import qualified FileCollector.Backend.Database.Types.Directory as Db
import qualified FileCollector.Backend.Database.Types.UploadRule as Db
import qualified FileCollector.Backend.Database.Types.User as Db
import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types.Directory
import           FileCollector.Common.Types.User

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

getDirectory' ::
  ( Db.MonadReadDirectory m
  , Db.MonadReadUser m
  , MonadLogger m
  )
  => User
  -> UserName
  -> DirectoryName
  -> m (Maybe Directory)
getDirectory' user ownerName dirName =
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
