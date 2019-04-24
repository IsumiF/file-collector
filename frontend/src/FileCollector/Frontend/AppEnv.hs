{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.AppEnv
  ( AppEnv
  , appEnv_baseUrl
  , appEnv_language
  , appEnv_userInfo
  ) where

import           Control.Lens
import           Data.Default
import           Data.Text                             (Text)
import           Reflex.Dom
import           Servant.Common.BaseUrl                (BaseUrl (BasePath))

import           FileCollector.Common.Types.UserInfo

data AppEnv t = AppEnv
  { _appEnv_baseUrl  :: BaseUrl
  , _appEnv_language :: Dynamic t Text
  , _appEnv_userInfo :: Dynamic t (Maybe UserInfo)
  }

makeLenses ''AppEnv

instance Reflex t => Default (AppEnv t) where
  def = AppEnv (BasePath "/") (constDyn "zh-CN") (constDyn Nothing)
