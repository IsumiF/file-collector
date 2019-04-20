{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Frontend.Environment.BasicEnv
  ( BasicEnv
  , basicEnv_baseUrl
  ) where

import           Control.Lens
import           Data.Default
import           Data.Text      (Text)
import           Servant.Reflex (BaseUrl (BasePath))

data BasicEnv = BasicEnv
  { _basicEnv_baseUrl    :: BaseUrl
  } deriving Show

makeLenses ''BasicEnv

instance Default BasicEnv where
  def = BasicEnv (BasePath "/")
