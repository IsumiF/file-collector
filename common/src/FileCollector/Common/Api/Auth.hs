{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeOperators         #-}

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
import qualified Servant.Docs.Internal           as Docs

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

instance Docs.ToAuthInfo (BasicAuth "file-collector" UserUploader) where
  toAuthInfo _ = Docs.DocAuthentication "uploader" "must be at least uploader"

instance Docs.ToAuthInfo (BasicAuth "file-collector" UserCollector) where
  toAuthInfo _ = Docs.DocAuthentication "collector" "must be at least collector"

instance Docs.ToAuthInfo (BasicAuth "file-collector" UserAdmin) where
  toAuthInfo _ = Docs.DocAuthentication "admin" "administrator"
