{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module FileCollector.Frontend.Environment.Preference
  ( Preference
  , preference_language
  ) where

import           Control.Lens
import           Data.Default
import           Data.Text    (Text)

data Preference = Preference
  { _preference_language :: Text
  }

makeLenses ''Preference

instance Default Preference where
  def = Preference "zh-cn"
