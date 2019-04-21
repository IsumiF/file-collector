{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Frontend.UI.LanguageChooser
  ( languageChooser
  ) where

import           Data.Text  (Text)
import           Reflex.Dom

languageChooser :: DomBuilder t m
                => Text -- ^default language code
                -> [(Text, Text)] -- ^supported language codes and their labels
                -> m (Dynamic t Text)
languageChooser langsWithCodes = do
  pure undefined
  -- elClass "div" "dropdown" $ do
