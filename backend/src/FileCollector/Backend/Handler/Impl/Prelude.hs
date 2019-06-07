module FileCollector.Backend.Handler.Impl.Prelude
  ( throw404OnNothing
  ) where

import FileCollector.Backend.Handler.AppHandler
import Servant.Server

throw404OnNothing :: AppHandler (Maybe a) -> AppHandler a
throw404OnNothing action = do
    maybeResult <- action
    case maybeResult of
      Nothing     -> throwError err404
      Just result -> pure result
