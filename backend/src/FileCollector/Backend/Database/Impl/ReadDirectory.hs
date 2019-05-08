{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Database.Impl.ReadDirectory
  ( getDirectory
  , getDirectoryId
  , getOwnDirectories
  , getVisibleDirectories
  , getAllDirectories
  , isDirectoryVisible
  ) where

import Data.Maybe (isJust)

import FileCollector.Backend.Database.Impl.Internal.Prelude
import FileCollector.Backend.Database.Types.CanUploadTo
import FileCollector.Backend.Database.Types.Directory
import FileCollector.Backend.Database.Types.User

getDirectory :: MonadSqlDb m
             => Text -- ^owner name
             -> Text -- ^directory name
             -> m (Maybe Directory)
getDirectory ownerName dirName = liftPersist $ do
    maybeOwnerId <- (fmap . fmap) entityKey (getBy (UniqueUserName ownerName))
    case maybeOwnerId of
      Nothing -> pure Nothing
      Just ownerId -> do
        maybeEntityDirectory <- getBy (UniqueOwnerDir ownerId dirName)
        pure $ fmap entityVal maybeEntityDirectory

getDirectoryId :: MonadSqlDb m
               => Text -- ^owner name
               -> Text -- ^directory name
               -> m (Maybe DirectoryId)
getDirectoryId ownerName dirName = liftPersist $ do
    maybeEntityOwner <- getBy (UniqueUserName ownerName)
    case maybeEntityOwner of
      Nothing -> pure Nothing
      Just entityOwner -> do
        let ownerId = entityKey entityOwner
        (fmap . fmap) entityKey $ getBy (UniqueOwnerDir ownerId dirName)

getOwnDirectories :: MonadSqlDb m
                  => Text
                  -> m [Directory]
getOwnDirectories ownerName = liftPersist $ do
    maybeEntityOwner <- getBy (UniqueUserName ownerName)
    let maybeOwnerId = fmap entityKey maybeEntityOwner
    case maybeOwnerId of
      Nothing -> pure []
      Just ownerId -> do
        entityDirs <- selectList [DirectoryOwner ==. ownerId] []
        pure $ fmap entityVal entityDirs

getVisibleDirectories :: MonadSqlDb m
                      => Text
                      -> m [Directory]
getVisibleDirectories uploaderName = liftPersist $ do
    maybeEntityUploader <- getBy (UniqueUserName uploaderName)
    let maybeUploaderId = fmap entityKey maybeEntityUploader
    let getVisibleDirsOfId uploaderId = do
          entities :: [(Entity CanUploadTo, Entity Directory)] <- [sqlQQ|
            SELECT ??, ?? FROM ^{CanUploadTo}
              INNER JOIN ^{Directory}
              ON ^{Directory}.@{DirectoryId} = ^{CanUploadTo}.@{CanUploadToId}
              WHERE ^{CanUploadTo}.@{CanUploadToUser} = #{uploaderId}
          |]
          pure $ fmap (entityVal . snd) entities
    maybe (pure []) getVisibleDirsOfId maybeUploaderId

getAllDirectories :: MonadSqlDb m => m [Directory]
getAllDirectories = liftPersist $ do
    dirEntities <- selectList [] []
    pure $ fmap entityVal dirEntities

isDirectoryVisible :: MonadSqlDb m
                   => Text -- ^user name
                   -> DirectoryId -- ^dir id
                   -> m Bool
isDirectoryVisible username dirId = liftPersist $ do
    maybeUserId <- (fmap . fmap) entityKey $ getBy (UniqueUserName username)
    case maybeUserId of
      Nothing -> pure False
      Just userId -> do
        maybeEntity <- selectFirst
          [CanUploadToUser ==. userId, CanUploadToDirectory ==. dirId] []
        pure $ isJust maybeEntity
