{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Frontend.Environment.BasicEnv
  ( BasicEnv
  , basicEnv_baseUrl
  , basicEnv_preference
  , module FileCollector.Frontend.Environment.Preference
  ) where

import           Control.Lens
import           Data.Default
import           Data.Text                                     (Text)
import           Reflex.Dom
import           Servant.Reflex                                (BaseUrl (BasePath))

import           FileCollector.Frontend.Environment.Preference

data BasicEnv t = BasicEnv
  { _basicEnv_baseUrl    :: BaseUrl
  , _basicEnv_preference :: Dynamic t Preference
  }

makeLenses ''BasicEnv

instance Reflex t => Default (BasicEnv t) where
  def = BasicEnv (BasePath "/") (constDyn def)
