{-# LANGUAGE RecursiveDo #-}

module FileCollector.Frontend.UI.Component.Dialog
  ( dialog
  , DialogStyle(..)
  , popupMessage
  ) where

import Data.Text (Text)
import Reflex.Dom

import FileCollector.Frontend.UI.Component.Button

dialog :: MonadWidget t m
       => DialogStyle
       -> Event t b
       -> m a
       -> m a
dialog style showEvt content = mdo
    (modalClassDyn, r) <- elDynAttr "div" (fmap ("class" =:) modalClassDyn) $ do
      divClass "modal-background" blank
      r' <- divClass "modal-content" $
        divClass ("notification " <> dialogStyleClass style) content
      (closeEvt, _) <- buttonClassAttr "modal-close is-large" ("aria-label" =: "close") blank
      showDyn <- holdDyn False $ leftmost [fmap (const True) showEvt, fmap (const False) closeEvt]
      let modalClassDyn' = ffor showDyn $ \toShow ->
            "modal" <> if toShow then " is-active" else ""
      pure (modalClassDyn', r')
    pure r

data DialogStyle = DialogStyleDefault
                 | DialogStyleInfo
                 | DialogStyleSuccess
                 | DialogStyleWarning
                   deriving (Show, Eq)

dialogStyleClass :: DialogStyle -> Text
dialogStyleClass DialogStyleInfo = "is-info"
dialogStyleClass DialogStyleSuccess = "is-success"
dialogStyleClass DialogStyleWarning = "is-warning"
dialogStyleClass DialogStyleDefault = ""

popupMessage :: MonadWidget t m
             => DialogStyle
             -> Event t a
             -> Dynamic t Text
             -> m ()
popupMessage style showEvt msgDyn = dialog style showEvt (dynText msgDyn)
