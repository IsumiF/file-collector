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
import           Language.Javascript.JSaddle.Types          (JSM)
import           Reflex.Dom                                 hiding
                                                             (mainWidgetWithHead)
import           Reflex.Dom.Main                            (mainWidgetWithHead)
import           Servant.Common.BaseUrl                     (BaseUrl (BaseFullUrl),
                                                             Scheme (Http))

import           FileCollector.Frontend.Environment.UserEnv
import           FileCollector.Frontend.UI.LanguageChooser  (languageChooser)
import           FileCollector.Frontend.UI.Login            (loginWidget)

jsmMain :: JSM ()
jsmMain = mainWidgetWithHead headElement bodyElement

headElement :: DomBuilder t m => m ()
headElement = do
  elAttr "meta" [("charset", "utf-8")] blank
  elAttr "meta"
    ( ("name" =: "viewport")
   <> ("content" =: "width=device-width, initial-scale=1, shrink-to-fit=no")
    ) blank
  elAttr "link" (("rel" =: "stylesheet") <> ("href" =: "styles/all.css")) blank

bodyElement :: DomBuilder t m => m ()
bodyElement =
  primaryWidget

primaryWidget :: forall t m. DomBuilder t m => m ()
primaryWidget = do
  -- let basicEnv :: BasicEnv =
  --       def & basicEnv_baseUrl .~ BaseFullUrl Http "127.0.0.1" 8081 "/"
  languageChooser undefined undefined
  pure ()
