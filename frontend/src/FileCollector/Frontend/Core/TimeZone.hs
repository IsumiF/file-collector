module FileCollector.Frontend.Core.TimeZone
  ( getBrowserTimeZone
  , formatUTCTime
  , showUTCTime
  ) where

import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified GHCJS.DOM as Dom
import qualified GHCJS.DOM.Window as DomWindow
import           Language.Javascript.JSaddle
import           Reflex.Dom

import qualified FileCollector.Frontend.Class.MonadTimeZone as MonadTimeZone
import FileCollector.Frontend.Class.MonadTimeZone (MonadTimeZone)

getBrowserTimeZone :: MonadWidget t m
                   => Event t a
                   -> m (Event t TimeZone)
getBrowserTimeZone triggerEvt =
    performEvent $ ffor triggerEvt $ \_ -> do
      window <- Dom.currentWindowUnchecked
      timeZoneOffset' <- liftJSM $ DomWindow.unWindow window ^. js0 ("fc_getTimeZoneOffset" :: String)
      timeZoneOffset <- liftJSM $ fromJSValUnchecked timeZoneOffset'
      pure $ minutesToTimeZone (- timeZoneOffset)

formatUTCTime :: TimeZone -> UTCTime -> Text
formatUTCTime timeZone time = T.pack $
    formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) zonedTime
  where
    zonedTime = utcToZonedTime timeZone time

showUTCTime :: MonadTimeZone t m
            => Dynamic t UTCTime
            -> m (Dynamic t Text)
showUTCTime utcTimeDyn = do
    tzDyn <- MonadTimeZone.getCurrentTimeZone
    pure $ formatUTCTime <$> tzDyn <*> utcTimeDyn
