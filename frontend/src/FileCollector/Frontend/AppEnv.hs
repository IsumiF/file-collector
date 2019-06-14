{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.AppEnv
  ( AppEnv
  , mkAppEnv
  , appEnv_serviceAccessors
  , appEnv_language
  , appEnv_loggedUser
  , appEnv_timeZone
  ) where

import Control.Lens
import Data.Text (Text)
import Data.Time
import Reflex.Dom

import FileCollector.Frontend.Types.LoggedUser
import FileCollector.Frontend.Types.ServiceAccessors

data AppEnv t m = AppEnv
  { _appEnv_serviceAccessors :: ServiceAccessors t m
  , _appEnv_language         :: Dynamic t Text
  , _appEnv_loggedUser       :: Dynamic t (Maybe LoggedUser)
  , _appEnv_timeZone         :: Dynamic t TimeZone
  }

makeLenses ''AppEnv

mkAppEnv :: ServiceAccessors t m
         -> Dynamic t Text
         -> Dynamic t (Maybe LoggedUser)
         -> Dynamic t TimeZone
         -> AppEnv t m
mkAppEnv = AppEnv
