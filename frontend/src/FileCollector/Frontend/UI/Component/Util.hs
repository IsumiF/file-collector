module FileCollector.Frontend.UI.Component.Util
  ( replaceAttrClass
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)

replaceAttrClass :: Text
                 -> Map Text Text
                 -> Map Text Text
replaceAttrClass = Map.insert "class"
