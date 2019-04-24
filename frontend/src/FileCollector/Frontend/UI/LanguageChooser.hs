module FileCollector.Frontend.UI.LanguageChooser
  ( languageChooser
  ) where

import           Data.Foldable                              (traverse_)
import           Data.Map.Strict                            (Map)
import           Data.Text                                  (Text)
import           Reflex.Dom

import           FileCollector.Frontend.UI.Component.Button (buttonAttr,
                                                             buttonClassAttr)
import           FileCollector.Frontend.UI.Component.Div    (divAttr,
                                                             divClassAttr)

languageChooser :: MonadWidget t m
                => m (Dynamic t Text)
languageChooser = do
    divAttr ("id" =: "LanguageChooser_root") $ do
      e <- dropdown defLang (constDyn langsWithLabels) def
      pure $ value e
  where
    defLang = "zh-CN"
    langsWithLabels = [("en-US", "English (United States)"), ("zh-CN", "中文简体")]

