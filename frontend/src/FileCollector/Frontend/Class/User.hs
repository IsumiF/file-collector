{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.User
  ( HasUser(..)
  ) where

import           Control.Lens
import           Reflex.Dom

import           FileCollector.Common.Types.User
import           FileCollector.Frontend.AppEnv
import           FileCollector.Frontend.Types.LoggedUser

-- | The environment possibly contains a user
class HasUser t env where
  getUser :: env -> Dynamic t (Maybe User)

instance Reflex t => HasUser t (AppEnv t) where
  getUser env = (fmap . fmap) (view loggedUser_user) (env ^. appEnv_loggedUser)
