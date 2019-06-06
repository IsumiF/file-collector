{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module FileCollector.Frontend.Message
  ( renderMsgDyn
  , renderMsg
  , Login(..)
  , LoginMessage(..)
  , TopBar(..)
  , TopBarMessage(..)
  ) where

import Control.Monad.Reader
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Reflex (Dynamic, Reflex, constDyn)
import Text.Shakespeare.I18N (RenderMessage (..), mkMessage)

import FileCollector.Common.Types
import FileCollector.Frontend.Class.Language

data Login = Login
mkMessage "Login" "values/messages/login" "zh-CN"

data TopBar = TopBar
mkMessage "TopBar" "values/messages/topbar" "zh-CN"

renderMsgDyn ::
  ( MonadReader env m
  , HasLanguage t env
  , Reflex t
  , RenderMessage master msg
  ) => Proxy env
    -> master
    -> Dynamic t msg
    -> m (Dynamic t Text)
renderMsgDyn _ master msgDyn = do
    env <- ask
    let langDyn = getLanguage env
    pure $ (\lang msg -> renderMessage master [lang] msg) <$> langDyn <*> msgDyn

renderMsg ::
  ( MonadReader env m
  , HasLanguage t env
  , Reflex t
  , RenderMessage master msg
  ) => Proxy env
    -> master
    -> msg
    -> m (Dynamic t Text)
renderMsg p master msg = renderMsgDyn p master (constDyn msg)
