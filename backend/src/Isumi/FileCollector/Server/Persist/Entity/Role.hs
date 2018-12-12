{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Isumi.FileCollector.Server.Persist.Entity.Role
  ( Role(..)
  ) where

import Data.Aeson
    (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import Database.Persist.TH
import GHC.Generics

data Role = RoleUploader
          | RoleCollector
          | RoleAdmin
            deriving (Show, Read, Eq, Generic)

instance ToJSON Role where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Role where

instance Ord Role where
    compare lhs rhs = compare (toInt lhs) (toInt rhs)

toInt :: Role -> Int
toInt RoleUploader  = 0
toInt RoleCollector = 1
toInt RoleAdmin     = 2

derivePersistField "Role"

