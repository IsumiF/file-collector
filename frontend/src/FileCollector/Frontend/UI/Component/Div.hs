module FileCollector.Frontend.UI.Component.Div
  ( divAttr
  , divClassAttr
  ) where

import           Data.Map.Strict                          (Map)
import           Data.Text                                (Text)
import           Reflex.Dom

import           FileCollector.Frontend.UI.Component.Util (replaceAttrClass)

divAttr :: DomBuilder t m
        => Map Text Text
        -> m a
        -> m a
divAttr attr child =
  elAttr "div" attr child

divClassAttr :: DomBuilder t m
             => Text
             -> Map Text Text
             -> m a
             -> m a
divClassAttr classes attr = divAttr (replaceAttrClass classes attr)
