{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  , Unique (UniqueName)
  , module FileCollector.Backend.Database.Types.Role
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist (Unique)
import Database.Persist.TH
    (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import FileCollector.Backend.Database.Types.Role

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  password ByteString
  role Role
  UniqueName name
  deriving Show
|]
