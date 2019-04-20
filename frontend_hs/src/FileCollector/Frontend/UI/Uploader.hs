module FileCollector.Frontend.UI.Uploader
  ( uploaderPage
  ) where

import           Control.Monad.Reader
import           FileCollector.Frontend.Environment.UserEnv
import           Reflex.Dom

uploaderPage :: MonadWidget t m => ReaderT UserEnv m ()
uploaderPage = undefined
