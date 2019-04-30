{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.UI.Login
  ( loginWidget
  ) where

import           Control.Monad.Reader
import           Data.Map.Strict                         (Map)
import           Data.Text                               (Text)
import           Reflex.Dom

import           FileCollector.Frontend.Class.Language
import           FileCollector.Frontend.Types.LoggedUser

loginWidget :: (MonadWidget t m, MonadReader env m, HasLanguage env t)
            => m (Dynamic t LoggedUser)
loginWidget =
  elAttr "div" [ ("class", "col-sm-12"), ("style", "text-align: center;") ] $
    elAttr "form" [ ("style", "display: inline-block;") ] $ do
      let dynAttrFormControl = constDyn [("class", "form-control")]
      elClass "div" "form-group" $
        elAttr "label" [ ("style", "text-align: left;") ] $ do
          text "Username"
          usernameInput <- textInput $
            def & textInputConfig_attributes .~ dynAttrFormControl
          pure ()
      elClass "div" "form-group" $
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
