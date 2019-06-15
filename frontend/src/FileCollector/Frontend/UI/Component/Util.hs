module FileCollector.Frontend.UI.Component.Util
  ( replaceAttrClass
  , concatM
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

replaceAttrClass :: Text
                 -> Map Text Text
                 -> Map Text Text
replaceAttrClass = Map.insert "class"

concatM :: (Monoid a, Applicative m) => [m a] -> m a
concatM x = fmap mconcat (sequenceA x)
