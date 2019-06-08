{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.AppEnv
  ( AppEnv
  , mkAppEnv
  , appEnv_serviceAccessors
  , appEnv_language
  , appEnv_loggedUser
  ) where

import Control.Lens
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

mkAppEnv :: ServiceAccessors t m
         -> Dynamic t Text
         -> Dynamic t (Maybe LoggedUser)
         -> AppEnv t m
mkAppEnv = AppEnv
