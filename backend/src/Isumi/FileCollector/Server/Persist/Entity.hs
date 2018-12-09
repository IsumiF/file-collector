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
  , User(..)
  , UserId
  , Directory(..)
  , DirectoryId
  , File(..)
  , FileId
  , migrateAll
  ) where

import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( UTCTime
    )
import Database.Persist.TH
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
  UniquePath owner name
  deriving Show
File
  hash ByteString
  name Text
  directory DirectoryId
  uploader UserId
  lastModifiedTime UTCTime default=now()
  location FileLocation
  UniqueHash hash
  deriving Show
|]

