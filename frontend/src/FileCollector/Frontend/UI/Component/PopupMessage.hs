{-# LANGUAGE RecursiveDo #-}

module FileCollector.Frontend.UI.Component.PopupMessage
  ( popupMessage
  ) where

import Data.Text (Text)
import Reflex.Dom

import FileCollector.Frontend.UI.Component.Button

popupMessage :: MonadWidget t m
             => Dynamic t Text
             -> Event t a
             -> m ()
popupMessage msgDyn showEvt = mdo
    modalClassDyn <- elDynAttr "div" (fmap ("class" =:) modalClassDyn) $ do
      divClass "modal-background" blank
      divClass "modal-content" $
        divClass "notification is-info" $
          dynText msgDyn
      (closeEvt, _) <- buttonClassAttr "modal-close is-large" ("aria-label" =: "close") blank
      showDyn <- holdDyn False $ leftmost [fmap (const True) showEvt, fmap (const False) closeEvt]
      let modalClassDyn' = ffor showDyn $ \toShow ->
            "modal" <> if toShow then " is-active" else ""
      pure modalClassDyn'
    blank

