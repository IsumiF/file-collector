{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Frontend.Monad.Language
  ( MonadHasLanguage(..)
  ) where

import           Control.Monad.State.Strict
import           Data.Text                                     (Text)
import           FileCollector.Frontend.Environment.UserConfig

class Monad m => MonadHasLanguage m where
  getLanguage :: m Text
  setLanguage :: Text -> m ()

newtype Wrapper m a = Wrapper
  { unWrap :: m a
  }

instance (MonadState UserConfig m) => MonadHasLanguage (Wrapper m) where
  getLanguage = Wrapper $ do
    config <- get
    pure $ view userConfig_language config
  setLanguage lang = Wrapper $
    modify (set userConfig_language lang)
