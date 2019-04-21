module FileCollector.Frontend.UI.Component.Button
  ( buttonAttr
  , buttonClassAttr
  ) where

import Data.Map.Strict (Map)
import Data.Text(Text)
import qualified Data.Map.Strict as Map

buttonAttr :: DomBuilder t m
           => Map Text Text -- ^attributes
           -> m a
           -> m (Event t (), a)
buttonAttr attr children = do
  (e, x) <- elAttr' "button" attr children
  pure $ (domEvent Click e, x)

buttonClassAttr :: DomBuilder t m
                => Text -- ^classes
                -> Map Text Text -- ^attributes
                -> m a
                -> m (Event t (), a)
buttonClassAttr classes attributes =
  buttonAttr (Map.insert "class" classes attributes)
