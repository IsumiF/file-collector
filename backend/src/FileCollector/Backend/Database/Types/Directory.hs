module FileCollector.Backend.Database.Types.Directory
  ( Directory(..)
  , DirectoryId
  , Unique (UniqueOwnerDir)
  , EntityField
    ( DirectoryName
    , DirectoryOwner
    , DirectoryExpirationTime
    , DirectoryUploadRules
    )
  ) where

import FileCollector.Backend.Database.Types.Internal
