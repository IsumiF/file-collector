{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.UI.UserControl
  ( userControl
  ) where

import           Control.Monad.Reader
import           Data.Functor                               (void)
import           Data.Map.Strict                            (Map)
import           Data.Text                                  (Text)
import           Reflex.Dom

import           FileCollector.Frontend.Class.Language
import           FileCollector.Frontend.Class.User
import           FileCollector.Frontend.UI.Component.Button (buttonAttr)

userControl :: forall t m env.
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , HasUser t env
  ) =>
    m ()
userControl = mdo
    dropdownEvt' <- elDynAttr "div" (fmap ("id" =: "UserControl_root" <>) rootAttrIsActive) $ do
      (dropdownEvt, _) <- elClass "div" "dropdown-trigger" $
        buttonAttr
          ( "class" =: "button"
         <> "aria-haspopup" =: "true"
         <> "aria-controls" =: "dropdown-menu"
          ) $
          elAttr "span" ("id" =: "UserControl_icon") $
            elClass "i" "fas fa-user-circle fa-3x" blank
      elAttr "div" ("id" =: "UserControl_menu" <> "role" =: "menu") $
        elClass "div" "dropdown-content" $ do
          elAttr "div" ("id" =: "UserControl_name") $ text "Logged in as"
          elClass "hr" "dropdown-divider" blank
          elClass "div" "dropdown-item" $
            buttonAttr ("id" =: "UserControl_logout") $ text "Logout"
      pure dropdownEvt
    

    rootAttrIsActive <-
      foldDyn (const not) False dropdownEvt' >>=
        pure . fmap (\p -> if p then ("class" :: Text) =: ("is-active" :: Text) else mempty)
    pure ()
