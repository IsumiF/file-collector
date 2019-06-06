{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module FileCollector.Frontend.UI.UserControl
  ( userControl
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Reflex.Dom

import FileCollector.Common.Types.User
import FileCollector.Frontend.Class.Language
import FileCollector.Frontend.Class.User
import FileCollector.Frontend.Message
import FileCollector.Frontend.UI.Component.Button (buttonAttr)

userControl :: forall t m env.
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , HasUser t env
  ) =>
    m (Event t ()) -- ^logout event
userControl = mdo
    (dropdownEvt', logoutEvt') <- elDynAttr "div" (fmap ("id" =: "UserControl_root" <>) rootAttrIsActive) $ do
      (dropdownEvt, _) <- elClass "div" "dropdown-trigger" $
        buttonAttr
          ( "class" =: "button"
         <> "aria-haspopup" =: "true"
         <> "aria-controls" =: "dropdown-menu"
          ) $
          elAttr "span" ("id" =: "UserControl_icon") $
            elClass "i" "fas fa-user-circle fa-3x" blank
      logoutEvt <- elAttr "div" ("id" =: "UserControl_menu" <> "role" =: "menu") $
        elClass "div" "dropdown-content" $ do
          elAttr "div" ("id" =: "UserControl_name") $ dynText greetingMsgTxt
          elClass "hr" "dropdown-divider" blank
          elClass "div" "dropdown-item" $
            fmap fst $ buttonAttr ("id" =: "UserControl_logout") $ text "Logout"
      pure (dropdownEvt, logoutEvt)

    rootAttrIsActive <-
      foldDyn (const not) False dropdownEvt' >>=
        pure . fmap (\p -> if p then ("class" :: Text) =: ("is-active" :: Text) else mempty)

    (userMaybeDyn :: Dynamic t (Maybe User)) <- asks getUser
    greetingMsgTxt <- renderMsgDyn envProxy TopBar $
      fmap (maybe MsgNotLoggedIn MsgLoggedInAs . fmap (^. user_name)) userMaybeDyn

    pure logoutEvt'
  where
    envProxy = Proxy :: Proxy env
