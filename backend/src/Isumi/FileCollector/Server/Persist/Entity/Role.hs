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

import Database.Persist.TH

data Role = RoleUploader
          | RoleCollector
          | RoleAdmin
            deriving (Show, Read, Eq)

derivePersistField "Role"

