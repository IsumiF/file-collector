{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module FileCollector.Backend.Core.BasicAuth
  ( authCheck
  ) where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Text.Encoding as Text
import           Servant (BasicAuthData (..), BasicAuthResult (..))

import           FileCollector.Backend.Core.Password (checkPassword)
import           FileCollector.Backend.Core.User (userDbToCommon)
import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
    (Backend, MonadConnection, withConnection)
import qualified FileCollector.Backend.Database.Class.MonadUser as Db
    (MonadUser, getUserByName)
import qualified FileCollector.Backend.Database.Types.User as Db (userPassword)
import           FileCollector.Common.Api.Auth (UserAuthWrapper, minRole, wrap)
import           FileCollector.Common.Types.User

authCheck :: forall backend m usr.
  ( Db.MonadUser (ReaderT backend m)
  , Db.MonadConnection m
  , Db.Backend m ~ backend
  , UserAuthWrapper usr
  )
  => BasicAuthData
  -> m (BasicAuthResult usr)
authCheck (BasicAuthData nameBs password) = do
    maybeDbUser <- Db.withConnection $ Db.getUserByName (Text.decodeUtf8 nameBs)
    case maybeDbUser of
      Nothing -> pure NoSuchUser
      Just dbUser -> do
        let cipherPassword = Db.userPassword dbUser
        if not (checkPassword password cipherPassword)
        then pure BadPassword
        else do
          let user = userDbToCommon dbUser
              role = user ^. user_role
          if role < minRole'
          then pure Unauthorized
          else pure . Authorized . wrap $ user
  where
    minRole' = minRole @usr
