{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.Main
  ( jsmMain
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Language.Javascript.JSaddle.Types             (JSM)
import           Reflex.Dom                                    hiding (mainWidgetWithHead)
import           Reflex.Dom.Main                               (mainWidgetWithHead)
import           Servant.Common.BaseUrl                        (BaseUrl (BaseFullUrl),
                                                                Scheme (Http))

import           FileCollector.Frontend.Environment.UserEnv
import           FileCollector.Frontend.UI.Login               (loginWidget)

jsmMain :: JSM ()
jsmMain = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  elAttr "meta" [("charset", "utf-8")] blank
  elAttr "link"
    [ ("rel", "shortcut icon")
    , ("type", "image/x-icon")
    , ("href", "static/favicon.ico")
    ] blank
  elAttr "meta"
    [ ("name", "viewport")
    , ("content", "width=device-width, initial-scale=1, shrink-to-fit=no")
    ] blank
  elAttr "link"
    [ ("rel", "stylesheet")
    , ("href", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
    , ("integrity", "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T")
    , ("crossorigin", "anonymous")
    ] blank

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  primaryWidget
  elAttr "script"
    [ ("src", "https://code.jquery.com/jquery-3.3.1.slim.min.js")
    , ("integrity", "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo")
    , ("crossorigin", "anonymous")
    ] blank
  elAttr "script"
    [ ("src", "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js")
    , ("integrity", "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1")
    , ("crossorigin", "anonymous")
    ] blank
  elAttr "script"
    [ ("src", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js")
    , ("integrity", "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM")
    , ("crossorigin", "anonymous")
    ] blank

primaryWidget :: forall t m. MonadWidget t m => m ()
primaryWidget = do
  -- let basicEnv :: BasicEnv =
  --       def & basicEnv_baseUrl .~ BaseFullUrl Http "127.0.0.1" 8081 "/"
  
  pure ()
