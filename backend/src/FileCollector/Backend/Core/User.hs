{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module FileCollector.Backend.Core.User
  ( getUser
  , userDbToCommon
  ) where

import Control.Monad.Reader
import Control.Lens

import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
    (Backend, MonadConnection, withConnection)
import qualified FileCollector.Backend.Database.Class.MonadReadUser as Db
    (MonadReadUser, getUserByName)
import qualified FileCollector.Backend.Database.Types.Role as Db (toCommonRole)
import qualified FileCollector.Backend.Database.Types.User as Db (User (..))
import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types.User

getUser ::
  ( Db.MonadConnection m
  , Db.Backend m ~ backend
  , Db.MonadReadUser (ReaderT backend m)
  )
  => User
  -> UserName
  -> m (Maybe User)
getUser me name =
    if me ^. user_role == RoleAdmin || me ^. user_name == name
    then Db.withConnection (getUserByName name)
    else pure Nothing
getUserByName ::
  ( Db.MonadReadUser m
  )
  => UserName
  -> m (Maybe User)
getUserByName name = do
    maybeDbUser <- Db.getUserByName (convert name)
    pure $ userDbToCommon <$> maybeDbUser

-- | Convert database user to common user, discarding password
userDbToCommon :: Db.User -> User
userDbToCommon dbUser = User
    (UserName (Db.userName dbUser))
    ((Db.toCommonRole . Db.userRole) dbUser)
