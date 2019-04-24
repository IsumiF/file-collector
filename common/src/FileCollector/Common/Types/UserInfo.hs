{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Common.Types.UserInfo
  ( UserInfo
  , newUserInfo
  , userInfo_user
  , userInfo_credential
  ) where

import           Control.Lens

import           FileCollector.Common.Types.Credential
import           FileCollector.Common.Types.User

data UserInfo = UserInfo
  { _userInfo_user       :: User
  , _userInfo_credential :: Credential
  } deriving Show

makeLenses ''UserInfo

newUserInfo :: User
            -> Credential
            -> UserInfo
newUserInfo = UserInfo
