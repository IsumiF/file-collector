module FileCollector.Frontend.UI.Component.Button
  ( buttonAttr
  , buttonClassAttr
  , buttonDynAttr
  ) where

import           Data.Map.Strict                          (Map)
import           Data.Text                                (Text)
import           Reflex.Dom

import           FileCollector.Frontend.UI.Component.Util (replaceAttrClass)

buttonAttr :: DomBuilder t m
           => Map Text Text -- ^attributes
           -> m a
           -> m (Event t (), a)
buttonAttr attr children = do
  (e, x) <- elAttr' "button" attr children
  pure (domEvent Click e, x)

buttonClassAttr :: DomBuilder t m
                => Text -- ^classes
                -> Map Text Text -- ^attributes
                -> m a
                -> m (Event t (), a)
buttonClassAttr classes attrs =
  buttonAttr (replaceAttrClass classes attrs)

buttonDynAttr :: (DomBuilder t m, PostBuild t m)
              => Dynamic t (Map Text Text)
              -> m a
              -> m (Event t (), a)
buttonDynAttr attrDyn sub = do
    (e, r) <- elDynAttr' "button" attrDyn sub
    pure (domEvent Click e, r)
