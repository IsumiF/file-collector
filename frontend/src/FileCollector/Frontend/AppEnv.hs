{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.AppEnv
  ( AppEnv
  , appEnv_baseUrl
  , appEnv_language
  , appEnv_loggedUser
  ) where

import           Control.Lens
import           Data.Default
import           Data.Text                               (Text)
import           Reflex.Dom
import           Servant.Common.BaseUrl                  (BaseUrl (BasePath))

import           FileCollector.Frontend.Types.LoggedUser

data AppEnv t = AppEnv
  { _appEnv_baseUrl    :: BaseUrl
  , _appEnv_language   :: Dynamic t Text
  , _appEnv_loggedUser :: Dynamic t (Maybe LoggedUser)
  }

makeLenses ''AppEnv

instance Reflex t => Default (AppEnv t) where
  def = AppEnv (BasePath "/") (constDyn "zh-CN") (constDyn Nothing)
