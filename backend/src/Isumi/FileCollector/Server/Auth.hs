{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Isumi.FileCollector.Server.Auth
  ( authContext
  , AuthContext
  , AuthContextEntries
  , UserUploader(..)
  , UserCollector(..)
  , UserAdmin(..)
  ) where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sql (runSqlPool)
import Isumi.FileCollector.Server.Persist (IsDbOp)
import Isumi.FileCollector.Server.Persist.Entity (Role (..), User (..))
import Isumi.FileCollector.Server.Persist.User (checkCredential)
import Isumi.FileCollector.Server.SqlConnPool
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
authCheckUploader = authCheckRole RoleUploader UserUploader

authCheckCollector :: BasicAuthCheck UserCollector
authCheckCollector = authCheckRole RoleCollector UserCollector

authCheckAdmin :: BasicAuthCheck UserAdmin
authCheckAdmin = authCheckRole RoleAdmin UserAdmin

authCheckRole :: Role -> (User -> a) -> BasicAuthCheck a
authCheckRole role userWrapper =
    BasicAuthCheck $ \(BasicAuthData name pwd) -> do
      userM <- runDbInIO $ checkCredential (decodeUtf8 name) pwd
      pure $ case userM of
        Nothing -> BadPassword
        Just user -> if userRole user >= role
                      then Authorized (userWrapper user)
                      else Unauthorized

runDbInIO :: forall a. (forall m. IsDbOp m => m a)
          -> IO a
runDbInIO op = do
    pool <- getSqlConnPool
    runSqlPool (runStdoutLoggingT op) pool

