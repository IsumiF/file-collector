{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module FileCollector.Frontend.Monad.Class.Language
  ( HasLanguage(..)
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Text                                  (Text)
import           Reflex.Dom

import           FileCollector.Frontend.Environment.UserEnv

class HasLanguage env t where
  getLanguage :: env -> Dynamic t Text

instance Reflex t => HasLanguage (BasicEnv t) t where
  getLanguage :: BasicEnv t -> Dynamic t Text
  getLanguage env =
    fmap (view preference_language) (view basicEnv_preference env)

-- instance Reflex t => HasLanguage (UserEnv t) where
--   getLanguage env = fmap (view preference_language) $ getLanguage (view userEnv_basicEnv env)
