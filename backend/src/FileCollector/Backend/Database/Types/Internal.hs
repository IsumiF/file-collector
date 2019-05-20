{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module FileCollector.Backend.Database.Types.Internal
  ( migrateAll
  , User(..)
  , UserId
  , module FileCollector.Backend.Database.Types.Role
  , Directory(..)
  , DirectoryId
  , File(..)
  , FileId
  , CanUploadTo(..)
  , CanUploadToId
  , PendingUploadFile(..)
  , PendingUploadFileId
  , Unique (UniqueUserName, UniqueOwnerDir, UniqueFile, UniquePendingUploadFile)
  , EntityField
    ( UserId
    , UserName
    , UserPassword
    , UserRole
    , DirectoryId
    , DirectoryName
    , DirectoryOwner
    , DirectoryExpirationTime
    , DirectoryUploadRules
    , FileId
    , FileName
    , FileHashValue
    , FileUploader
    , FileDirectory
    , FileLastModified
    , FileRawPath
    , CanUploadToId
    , CanUploadToUser
    , CanUploadToDirectory
    , PendingUploadFileName
    , PendingUploadFileNewName
    , PendingUploadFileUploader
    , PendingUploadFileDirectory
    , PendingUploadFileRequestTime
    , PendingUploadFileRawPath
    )
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH
    (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import           FileCollector.Backend.Database.Types.HashValue
import           FileCollector.Backend.Database.Types.Role
import           FileCollector.Backend.Database.Types.UploadRule (UploadRule)
import           FileCollector.Common.Base.Convertible
import qualified FileCollector.Common.Types.User as Common

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  password ByteString
  role Role
  UniqueUserName name
  deriving Show
Directory
  name Text
  owner UserId
  expirationTime UTCTime Maybe
  uploadRules [UploadRule]
  UniqueOwnerDir owner name
  deriving Show Eq
File
  name Text
  hashValue HashValue
  uploader UserId
  directory DirectoryId
  lastModified UTCTime
  rawPath ByteString
  UniqueFile directory uploader name
  deriving Show
CanUploadTo
  user UserId
  directory DirectoryId
  deriving Show
PendingUploadFile
  name Text 
  newName Text Maybe
  uploader UserId
  directory DirectoryId
  requestTime UTCTime
  rawPath ByteString
  UniquePendingUploadFile name uploader directory
  deriving Show
|]

instance Convertible User Common.User where
  convert dbUser = Common.User
    (Common.UserName (userName dbUser))
    ((convert . userRole) dbUser)
