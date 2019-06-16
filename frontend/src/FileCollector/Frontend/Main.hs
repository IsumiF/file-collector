{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.Main
  ( jsmMain
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Time (utc)
import Language.Javascript.JSaddle.Types (JSM)
import Reflex.Dom hiding (mainWidgetWithHead)
import Reflex.Dom.Main (mainWidgetWithHead)
import Servant.Common.BaseUrl (BaseUrl (BaseFullUrl), Scheme (Http))

import           FileCollector.Frontend.AppEnv
import           FileCollector.Frontend.Config
import qualified FileCollector.Frontend.Core.Login as Core
import           FileCollector.Frontend.Core.TimeZone (getBrowserTimeZone)
import           FileCollector.Frontend.Service (generateServiceAccessors)
import qualified FileCollector.Frontend.UI.FileExplorer as FileExplorer
import           FileCollector.Frontend.UI.Login (loginWidget)
import           FileCollector.Frontend.UI.TopBar (topBar)

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
  elAttr "script" ("src" =: "js/TimeZone.js") blank

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  primaryWidget
-- <script defer src="https://use.fontawesome.com/releases/v5.3.1/js/all.js"></script>
  elAttr "script"
    ( "src" =: "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
   <> "defer" =: "defer"
    ) blank

primaryWidget :: forall t m. MonadWidget t m => m ()
primaryWidget =
    case config of
      Nothing -> error "Failed to read compile time config"
      Just config' -> mdo
        (langDyn', loggedUserDyn') <- flip runReaderT appEnv $ do
          (langDyn, logoutEvt) <- topBar
          elClass "section" "section" $
            divClass "container" $ do
              loginEvt <- loginWidget
              FileExplorer.widget

              loggedUserDyn <- Core.combineUserEvent logoutEvt loginEvt
              pure (langDyn, loggedUserDyn)

        let baseUrl = BaseFullUrl
              Http
              (config' ^. config_serverIp)
              (config' ^. config_serverPort)
              ""
        sa <- runReaderT generateServiceAccessors baseUrl
        postBuildEvt <- getPostBuild
        timeZoneDyn <- getBrowserTimeZone postBuildEvt >>= holdDyn utc

        let appEnv = mkAppEnv sa langDyn' loggedUserDyn' timeZoneDyn

        blank
