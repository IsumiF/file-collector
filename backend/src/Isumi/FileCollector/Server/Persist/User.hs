{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Isumi.FileCollector.Server.Persist.User
  ( getRoleByName
  ) where

import Data.Text (Text)
import Database.Persist
import Isumi.FileCollector.Server.Persist.Entity
import Isumi.FileCollector.Server.Persist.Prelude

getRoleByName :: IsDbOp m
              => Text -> m (Maybe Role)
getRoleByName name = do
    entity <- liftPersist $ getBy $ UniqueUserName name
    pure $ (userRole . entityVal) <$> entity

