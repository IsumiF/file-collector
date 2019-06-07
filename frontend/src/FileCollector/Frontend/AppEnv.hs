{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.AppEnv
  ( AppEnv
  , appEnv_serviceAccessors
  , appEnv_language
  , appEnv_loggedUser
  ) where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Reflex.Dom

import FileCollector.Frontend.Types.LoggedUser
import FileCollector.Frontend.Types.ServiceAccessors

data AppEnv t m = AppEnv
  { _appEnv_serviceAccessors :: ServiceAccessors t m
  , _appEnv_language         :: Dynamic t Text
  , _appEnv_loggedUser       :: Dynamic t (Maybe LoggedUser)
  }

makeLenses ''AppEnv

instance Reflex t => Default (AppEnv t m) where
  def = AppEnv (error "Not implemented") (constDyn "zh-CN") (constDyn Nothing)
