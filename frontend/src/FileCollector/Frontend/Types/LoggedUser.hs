{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.Types.LoggedUser
  ( LoggedUser(..)
  , loggedUser_user
  , loggedUser_authData
  , User
  , BasicAuthData
  ) where

import           Control.Lens
import           Servant.API                     (BasicAuthData)

import           FileCollector.Common.Types.User

data LoggedUser = LoggedUser
  { _loggedUser_user     :: User
  , _loggedUser_authData :: BasicAuthData
  }

makeLenses ''LoggedUser
