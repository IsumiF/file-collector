{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Isumi.FileCollector.Server.Persist.User
  ( mkUser
  , checkCredential
  , getUserByName
  , User(..)
  , UserId
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist
import Isumi.FileCollector.Server.Password
import Isumi.FileCollector.Server.Persist.Entity
import Isumi.FileCollector.Server.Persist.Prelude

-- | Create a user. Hash password automatically.
mkUser :: Text -- ^name
       -> Role -- ^role
       -> Maybe Text -- ^display name
       -> ByteString -- ^password (in plain text)
       -> IO User
mkUser name role displayName password =
    User name role displayName <$> hashPwd password

checkCredential :: Database m
                => Text -- ^username
                -> ByteString -- ^password
                -> m (Maybe User)
checkCredential name pwd = do
    entity <- liftPersist $ getBy $ UniqueUserName name
    let user = entityVal <$> entity
        pwdCorrect = verifyPwd pwd <$> (userHashedPassword <$> user)
    pure $ do
      pwdCorrect_ <- pwdCorrect
      if pwdCorrect_ then user else Nothing

getUserByName :: Database m
              => Text -- ^name
              -> m (Maybe User)
getUserByName name = do
    entity <- liftPersist $ getBy $ UniqueUserName name
    pure $ entityVal <$> entity
    