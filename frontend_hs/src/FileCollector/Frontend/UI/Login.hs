{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.UI.Login
  ( loginWidget
  ) where

import           Control.Monad.Reader
import           Data.Map.Strict                            (Map)
import           Data.Text                                  (Text)
import           Reflex.Dom

import           FileCollector.Frontend.Environment.UserEnv
import           FileCollector.Frontend.Monad.Language

loginWidget :: (MonadWidget t m, MonadReader BasicEnv m, MonadHasLanguage m)
            => m (Dynamic t UserEnv)
-- loginWidget :: forall t m. MonadWidget t m => m ()
loginWidget = do
  elAttr "div" [ ("class", "col-sm-12"), ("style", "text-align: center;") ] $
    elAttr "form" [ ("style", "display: inline-block;") ] $ do
      let dynAttrFormControl = constDyn [("class", "form-control")]
      elClass "div" "form-group" $ do
        elAttr "label" [ ("style", "text-align: left;") ] $ do
          text "Username"
          usernameInput <- textInput $
            def & textInputConfig_attributes .~ dynAttrFormControl
          pure ()
      elClass "div" "form-group" $ do
        elAttr "label" [ ("style", "text-align: left;") ] $ do
          text "Password"
          passwordInput <- textInput $
            def & textInputConfig_inputType .~ "password"
                & textInputConfig_attributes .~ dynAttrFormControl
          pure ()
      (evLogin, _) <- elAttr "div" [ ("style", "width: 100%; text-align:center;") ] $
        buttonAttr
          [ ("class", "btn btn-primary" )
          ] $ text "Login"
      pure undefined

buttonAttr :: DomBuilder t m
           => Map Text Text -- attributes
           -> m a
           -> m (Event t (), a)
buttonAttr attr children = do
  (e, x) <- elAttr' "button" attr children
  pure $ (domEvent Click e, x)
