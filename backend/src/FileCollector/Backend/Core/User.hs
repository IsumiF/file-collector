{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module FileCollector.Backend.Core.User
  ( getUserByName
  , userDbToCommon
  ) where

import Control.Monad.Reader
import Data.Text (Text)

import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
    (Backend, MonadConnection, withConnection)
import qualified FileCollector.Backend.Database.Class.MonadUser as Db
    (MonadUser, getUserByName)
import qualified FileCollector.Backend.Database.Types.Role as Db (toCommonRole)
import qualified FileCollector.Backend.Database.Types.User as Db (User (..))
import           FileCollector.Common.Types.User

getUserByName ::
  ( Db.MonadConnection m
  , Db.MonadUser (ReaderT backend m)
  , Db.Backend m ~ backend
  )
  => Text
  -> m (Maybe User)
getUserByName name = do
    maybeDbUser <- Db.withConnection $ Db.getUserByName name
    pure $ userDbToCommon <$> maybeDbUser

userDbToCommon :: Db.User -> User
userDbToCommon dbUser = User
    (Db.userName dbUser)
    ((Db.toCommonRole . Db.userRole) dbUser)
