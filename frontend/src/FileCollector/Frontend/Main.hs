{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.Main
  ( jsmMain
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Default                      (def)
import           Language.Javascript.JSaddle.Types (JSM)
import           Reflex.Dom                        hiding (mainWidgetWithHead)
import           Reflex.Dom.Main                   (mainWidgetWithHead)
import           Servant.Common.BaseUrl            (BaseUrl (BaseFullUrl),
                                                    Scheme (Http))

import           FileCollector.Frontend.AppEnv
import           FileCollector.Frontend.UI.TopBar  (topBar)

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

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  primaryWidget
-- <script defer src="https://use.fontawesome.com/releases/v5.3.1/js/all.js"></script>
  elAttr "script"
    ( "src" =: "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
   <> "defer" =: "defer"
    ) blank

primaryWidget :: forall t m. MonadWidget t m => m ()
primaryWidget = mdo
    (langDyn, logoutEvt) <- elClass "div" "container" $ runReaderT topBar appEnv

    let appEnv :: AppEnv t =
          def & appEnv_baseUrl .~ BaseFullUrl Http "127.0.0.1" 8080 "/api/"
              & appEnv_language .~ langDyn
    pure ()
