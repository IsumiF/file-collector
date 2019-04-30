{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.Language
  ( HasLanguage(..)
  ) where

import           Control.Lens
import           Data.Text                     (Text)
import           Reflex.Dom

import           FileCollector.Frontend.AppEnv

class HasLanguage t env where
  getLanguage :: env -> Dynamic t Text

instance Reflex t => HasLanguage t (AppEnv t) where
  getLanguage :: AppEnv t -> Dynamic t Text
  getLanguage env = env ^. appEnv_language

instance Reflex t => HasLanguage t (Dynamic t Text) where
  getLanguage = id