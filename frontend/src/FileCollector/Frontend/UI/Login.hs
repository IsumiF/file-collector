{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.UI.Login
  ( loginWidget
  ) where

import Control.Lens hiding (children)
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Reflex.Dom

import           FileCollector.Common.Types
import           FileCollector.Frontend.Class.Language
import qualified FileCollector.Frontend.Class.Service.MonadGetUser as Service
    (MonadGetUser)
import           FileCollector.Frontend.Class.User
import qualified FileCollector.Frontend.Core.Login as Core
import           FileCollector.Frontend.Message (Login(..), LoginMessage (..), renderMsg)
import           FileCollector.Frontend.Types.LoggedUser

loginWidget ::
  forall t env m.
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , HasUser t env
  , Service.MonadGetUser t m
  ) => m (Event t LoggedUser)
loginWidget = do
    userDyn <- asks getUser
    ee <- dyn $ fmap (\user -> if isJust user then pure never else loginWidget') userDyn
    switchHold never ee
  where
    loginWidget' = do
      msgUsername <- renderMsg pEnv Login MsgUsername
      msgPassword <- renderMsg pEnv Login MsgPassword
      msgLogin <- renderMsg pEnv Login MsgLogin

      elAttr "div" [ ("class", "col-sm-12"), ("style", "text-align: center;") ] $
        elAttr "form" [ ("style", "display: inline-block;") ] $ do
          let dynAttrFormControl = constDyn [("class", "form-control")]
          usernameDyn <- elClass "div" "form-group" $
            elAttr "label" [ ("style", "text-align: left;") ] $ do
              dynText msgUsername
              usernameInput <- textInput $
                def & textInputConfig_attributes .~ dynAttrFormControl
              pure (value usernameInput)
          passwordDyn <- elClass "div" "form-group" $
            elAttr "label" [ ("style", "text-align: left;") ] $ do
              dynText msgPassword
              passwordInput <- textInput $
                def & textInputConfig_inputType .~ "password"
                    & textInputConfig_attributes .~ dynAttrFormControl
              pure (value passwordInput)
          (evLogin, _) <- elAttr "div" [ ("style", "width: 100%; text-align:center;") ] $
            buttonAttr
              [ ("class", "btn btn-primary" )
              , ("type", "button")
              ] $ dynText msgLogin
          
          let credDyn = (,) <$> fmap UserName usernameDyn <*> fmap Password passwordDyn
              credEvt = tagPromptlyDyn credDyn evLogin
          Core.login credEvt

    pEnv = Proxy :: Proxy env

buttonAttr :: DomBuilder t m
           => Map Text Text -- attributes
           -> m a
           -> m (Event t (), a)
buttonAttr attr children = do
  (e, x) <- elAttr' "button" attr children
  pure (domEvent Click e, x)
