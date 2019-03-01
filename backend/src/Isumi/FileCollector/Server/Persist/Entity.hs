{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Isumi.FileCollector.Server.Persist.Entity
  ( module Isumi.FileCollector.Server.Persist.Entity.Role
  , module Isumi.FileCollector.Server.Persist.Entity.FileLocation
  , User(..)
  , UserId
  , Directory(..)
  , DirectoryId
  , File(..)
  , FileId
  , Unique(..)
  , migrateAll
  , mkUser
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.TH
import Isumi.FileCollector.Server.Password
import Isumi.FileCollector.Server.Persist.Entity.FileLocation
import Isumi.FileCollector.Server.Persist.Entity.Role

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  role Role
  displayName Text Maybe default=NULL
  password ByteString
  UniqueUserName name
  deriving Show
Directory
  owner UserId
  name Text
  expireTime UTCTime Maybe
  UniqueDirOfOwner owner name
  deriving Show
File
  hash ByteString
  name Text
  directory DirectoryId
  uploader UserId
  lastModifiedTime UTCTime
  location FileLocation
  UniqueNameInDir directory name
  deriving Show
CanUploadTo
  user UserId
  directory DirectoryId
  deriving Show
|]

-- | Create a user. Hash password automatically, but require IO
mkUser :: Text -- ^name
       -> Role -- ^role
       -> Maybe Text -- ^display name
       -> ByteString -- ^password (in plain text)
       -> IO User
mkUser name role displayName password =
    User name role displayName <$> hashPwd password

