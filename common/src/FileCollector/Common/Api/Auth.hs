{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}

module FileCollector.Common.Api.Auth
  ( AuthUploader
  , AuthCollector
  , AuthAdmin
  , UserAuthWrapper(..)
  , UserUploader(..)
  , UserCollector(..)
  , UserAdmin(..)
  ) where

import           Servant.API

import           FileCollector.Common.Types.User

type Realm = "file-collector"

type AuthUploader = BasicAuth Realm UserUploader

newtype UserUploader = UserUploader User

type AuthCollector = BasicAuth Realm UserCollector

newtype UserCollector = UserCollector User

type AuthAdmin = BasicAuth Realm UserAdmin

newtype UserAdmin = UserAdmin User

class UserAuthWrapper a where
  unWrap :: a -> User
  wrap :: User -> a
  minRole :: Role

instance UserAuthWrapper UserUploader where
  unWrap (UserUploader u) = u
  wrap = UserUploader
  minRole = RoleUploader

instance UserAuthWrapper UserCollector where
  unWrap (UserCollector u) = u
  wrap = UserCollector
  minRole = RoleCollector

instance UserAuthWrapper UserAdmin where
  unWrap (UserAdmin u) = u
  wrap = UserAdmin
  minRole = RoleAdmin
