{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Isumi.FileCollector.Server.Auth
  ( authContext
  , AuthContext
  , AuthContextEntries
  , UserUploader(..)
  , UserCollector(..)
  , UserAdmin(..)
  ) where

import Isumi.FileCollector.Server.Persist.Entity (Role (..), User (..))
import Servant

type AuthContextEntries = '[
                     BasicAuthCheck UserUploader
                   , BasicAuthCheck UserCollector
                   , BasicAuthCheck UserAdmin
                   ]

type AuthContext = Context AuthContextEntries

authContext :: AuthContext
authContext =
       authCheckUploader
    :. authCheckCollector
    :. authCheckAdmin
    :. EmptyContext

newtype UserUploader = UserUploader User
newtype UserCollector = UserCollector User
newtype UserAdmin = UserAdmin User

authCheckUploader :: BasicAuthCheck UserUploader
authCheckUploader = BasicAuthCheck $ \(BasicAuthData name _) -> do
    putStrLn $ "uploader: " ++ show name
    if name /= "collector" && name /= "uploader"
    then do
      putStrLn "Can not upload"
      pure Unauthorized
    else pure . Authorized . UserUploader $ dumbUser

dumbUser :: User
dumbUser = User "name" RoleUploader Nothing "abc"

authCheckCollector :: BasicAuthCheck UserCollector
authCheckCollector = BasicAuthCheck $ \(BasicAuthData name _) -> do
    putStrLn $ "collector: " ++ show name
    if name /= "collector"
    then do
      putStrLn "Should FAIL !!!"
      pure NoSuchUser
    else pure . Authorized . UserCollector $ dumbUser

authCheckAdmin :: BasicAuthCheck UserAdmin
authCheckAdmin = undefined

