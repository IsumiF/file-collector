{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.Environment.UserEnv
  ( UserEnv
  , mkUserEnv
  , userEnv_basicEnv
  , userEnv_user
  , userEnv_credential
  , module FileCollector.Frontend.Environment.BasicEnv
  , module FileCollector.Common.Types.User
  , module FileCollector.Common.Types.Credential
  ) where

import           Control.Lens
import           FileCollector.Common.Types.Credential (Credential)
import           FileCollector.Common.Types.User       (User)
import           FileCollector.Frontend.Environment.BasicEnv(BasicEnv)

data UserEnv = UserEnv
  { _userEnv_basicEnv   :: BasicEnv
  , _userEnv_user       :: User
  , _userEnv_credential :: Credential
  } deriving Show

makeLenses ''UserEnv

mkUserEnv :: User
          -> Credential
          -> BasicEnv
          -> UserEnv
mkUserEnv user credential basicEnv = UserEnv basicEnv user credential
