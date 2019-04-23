{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Frontend.UI.LanguageChooser
  ( languageChooser
  ) where

import           Data.Text                                  (Text)
import           Reflex.Dom

import           FileCollector.Frontend.UI.Component.Button (buttonAttr,
                                                             buttonClassAttr)
import           FileCollector.Frontend.UI.Component.Div    (divAttr,
                                                             divClassAttr)

languageChooser :: DomBuilder t m
                => Text -- ^default language code
                -> [(Text, Text)] -- ^supported language codes and their labels
                -> m (Dynamic t Text)
languageChooser defLang langsWithLabels = do
    divAttr ("id" =: "LanguageChooser_rootDiv") $ do
      el "select" $ do
        el "option" $ text "First option"
        el "option" $ text "Second option"
    pure undefined
      
