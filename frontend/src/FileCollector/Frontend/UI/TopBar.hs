module FileCollector.Frontend.UI.TopBar
  ( topBar
  ) where

import           Control.Monad.Reader
import           Data.Text                                 (Text)
import           Reflex.Dom

import           FileCollector.Frontend.Class.Language
import           FileCollector.Frontend.Class.User
import           FileCollector.Frontend.UI.LanguageChooser (languageChooser)
import           FileCollector.Frontend.UI.UserControl     (userControl)

topBar ::
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , HasUser t env
  ) =>
    m (Dynamic t Text, Event t ()) -- ^(language, logout event)
topBar =
    elAttr "div" ("id" =: "TopBar_root") $ do
      elAttr "div" ("id" =: "TopBar_space") blank
      langDyn <- languageChooser
      logoutEvt <- userControl
      pure (langDyn, logoutEvt)
