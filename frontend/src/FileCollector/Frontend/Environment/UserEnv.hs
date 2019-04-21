{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.Environment.UserEnv
  ( UserEnv
  , newUserEnv
  , userEnv_basicEnv
  , userEnv_user
  , userEnv_credential
  , module FileCollector.Frontend.Environment.BasicEnv
  , module FileCollector.Common.Types.User
  , module FileCollector.Common.Types.Credential
  ) where

import           Control.Lens

import           FileCollector.Common.Types.Credential
import           FileCollector.Common.Types.User
import           FileCollector.Frontend.Environment.BasicEnv

data UserEnv t = UserEnv
  { _userEnv_basicEnv   :: BasicEnv t
  , _userEnv_user       :: User
  , _userEnv_credential :: Credential
  }

makeLenses ''UserEnv

newUserEnv :: User
           -> Credential
           -> BasicEnv t
           -> UserEnv t
newUserEnv user credential basicEnv = UserEnv basicEnv user credential
