{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Isumi.FileCollector.Server.Persist.User
  ( getRoleByName
  , checkCredential
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist
import Isumi.FileCollector.Server.Password
import Isumi.FileCollector.Server.Persist.Entity
import Isumi.FileCollector.Server.Persist.Prelude

getRoleByName :: IsDbOp m
              => Text -> m (Maybe Role)
getRoleByName name = do
    entity <- liftPersist $ getBy $ UniqueUserName name
    pure $ (userRole . entityVal) <$> entity

checkCredential :: IsDbOp m
                => Text -- ^username
                -> ByteString -- ^password
                -> m (Maybe User)
checkCredential name pwd = do
    entity <- liftPersist $ getBy $ UniqueUserName name
    let user = entityVal <$> entity
        pwdCorrect = verifyPwd pwd <$> (userPassword <$> user)
    pure $ do
      pwdCorrect_ <- pwdCorrect
      if pwdCorrect_ then user else Nothing


