{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}

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
  , Unique (UniqueUserName, UniqueOwnerDir, UniqueFile)
  , EntityField
    ( UserName
    , UserPassword
    , UserRole
    , DirectoryName
    , DirectoryOwner
    , DirectoryExpirationTime
    , DirectoryUploadRules
    , FileName
    , FileHashValue
    , FileUploader
    , FileDirectory
    , FileRawPath
    , CanUploadToUser
    , CanUploadToDirectory
    )
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH
    (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

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
  deriving Show
File
  name Text
  hashValue ByteString
  uploader UserId
  directory DirectoryId
  rawPath ByteString
  UniqueFile directory uploader name
  deriving Show
CanUploadTo
  user UserId
  directory DirectoryId
  deriving Show
|]

instance Convertible User Common.User where
  convert dbUser = Common.User
    (Common.UserName (userName dbUser))
    ((convert . userRole) dbUser)
