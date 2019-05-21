{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module FileCollector.Common.Api.User
  ( -- * Combined API
    Api
    -- * Inner APIs
  , ApiCreateUser
  , ApiGetUserList
  , ApiDeleteUser
  , ApiGetUser
  , ApiPutUser
  , ApiChangePasssword
  ) where

import           Servant.API
import qualified Servant.Docs as Docs

import FileCollector.Common.Api.Auth (AuthAdmin, AuthUploader)
import FileCollector.Common.Types.Password (Password)
import FileCollector.Common.Types.User (User, UserName)

type Api = "user" :>
  ( ApiCreateUser
  :<|> ApiGetUserList
  :<|> ApiDeleteUser
  :<|> ApiGetUser
  :<|> ApiPutUser
  :<|> ApiChangePasssword
  )

type ApiCreateUser =
  AuthAdmin
  :> ReqBody '[JSON] (User, Password) -- user and password
  :> Post '[JSON] ()

type ApiGetUserList =
  AuthAdmin
  :> Get '[JSON] [User]

type ApiDeleteUser =
  AuthAdmin
  :> Capture "name" UserName
  :> Delete '[JSON] ()

type ApiGetUser =
  AuthUploader
  :> Capture "name" UserName
  :> Get '[JSON] User

type ApiPutUser =
  AuthUploader
  :> Capture "name" UserName
  :> Put '[JSON] ()

type ApiChangePasssword =
  AuthUploader
  :> Capture "name" UserName
  :> "password"
  :> ReqBody '[JSON] Password -- new password
  :> Put '[JSON] ()

instance Docs.ToCapture (Capture "name" UserName) where
  toCapture _ =
    Docs.DocCapture "name" "Username"
