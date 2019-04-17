module FileCollector.Frontend.UI.Uploader
  ( uploaderPage
  ) where

import           Control.Monad.Reader
import           FileCollector.Frontend.AppEnv
import           Reflex.Dom

uploaderPage :: MonadWidget t m => ReaderT AppEnv m ()
uploaderPage = undefined
