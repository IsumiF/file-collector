{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Frontend.Environment.UserConfig
  ( UserConfig
  , userConfig_language
  ) where

import           Control.Lens
import           Data.Default
import           Data.Text    (Text)

data UserConfig = UserConfig
  { _userConfig_language :: Text
  } deriving Show

makeLenses ''UserConfig

instance Default UserConfig where
  def = UserConfig "zh-cn"
