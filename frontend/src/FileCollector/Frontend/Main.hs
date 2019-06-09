{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.Main
  ( jsmMain
  ) where

import Control.Monad.Reader
import Language.Javascript.JSaddle.Types (JSM)
import Reflex.Dom hiding (mainWidgetWithHead)
import Reflex.Dom.Main (mainWidgetWithHead)
import Servant.Common.BaseUrl (BaseUrl (BaseFullUrl), Scheme (Http))

import           FileCollector.Frontend.AppEnv
import qualified FileCollector.Frontend.Core.Login as Core
import           FileCollector.Frontend.Service (generateServiceAccessors)
import           FileCollector.Frontend.UI.Login (loginWidget)
import           FileCollector.Frontend.UI.TopBar (topBar)
import qualified FileCollector.Frontend.UI.FileExplorer as FileExplorer

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
    (langDyn', loggedUserDyn') <- flip runReaderT appEnv $ do
      (langDyn, logoutEvt) <- topBar
      elClass "section" "section" $
        divClass "container" $ do
          loginEvt <- loginWidget
          FileExplorer.widget

          loggedUserDyn <- Core.combineUserEvent logoutEvt loginEvt
          pure (langDyn, loggedUserDyn)

    let baseUrl = BaseFullUrl Http "127.0.0.1" 8080 ""
    sa <- runReaderT generateServiceAccessors baseUrl

    let appEnv = mkAppEnv sa langDyn' loggedUserDyn'
    
    blank
