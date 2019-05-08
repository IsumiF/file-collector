module FileCollector.Backend.Database.Types.Directory
  ( Directory(..)
  , DirectoryId
  , Unique (UniqueOwnerDir)
  , EntityField
    ( DirectoryId
    , DirectoryName
    , DirectoryOwner
    , DirectoryExpirationTime
    , DirectoryUploadRules
    )
  ) where

import FileCollector.Backend.Database.Types.Internal
