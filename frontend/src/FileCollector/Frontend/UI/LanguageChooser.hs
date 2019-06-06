module FileCollector.Frontend.UI.LanguageChooser
  ( languageChooser
  ) where

import Data.Text (Text)
import Reflex.Dom

import FileCollector.Frontend.UI.Component.Div (divAttr)

languageChooser :: MonadWidget t m
                => m (Dynamic t Text)
languageChooser =
    divAttr ("id" =: "LanguageChooser_root") $ do
      e <- dropdown defLang (constDyn langsWithLabels) def
      pure $ value e
  where
    defLang = "zh-CN"
    langsWithLabels = [("en-US", "English (United States)"), ("zh-CN", "中文简体")]
