module FileCollector.Frontend.Message.Prelude
  ( renderMsgDyn
  , renderMsg
  , module Control.Monad.Reader
  , Proxy(..)
  , Text
  , Dynamic
  , Reflex
  , constDyn
  , module Text.Shakespeare.I18N
  , module FileCollector.Common.Types
  , module FileCollector.Frontend.Class.Language
  ) where

import Control.Monad.Reader
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Reflex (Dynamic, Reflex, constDyn)
import Text.Shakespeare.I18N (RenderMessage (..), mkMessage)

import FileCollector.Common.Types
import FileCollector.Frontend.Class.Language

renderMsgDyn ::
  ( MonadReader env m
  , HasLanguage t env
  , Reflex t
  , RenderMessage master msg
  ) => Proxy env
    -> master
    -> Dynamic t msg
    -> m (Dynamic t Text)
renderMsgDyn _ master msgDyn = do
    env <- ask
    let langDyn = getLanguage env
    pure $ (\lang msg -> renderMessage master [lang] msg) <$> langDyn <*> msgDyn

renderMsg ::
  ( MonadReader env m
  , HasLanguage t env
  , Reflex t
  , RenderMessage master msg
  ) => Proxy env
    -> master
    -> msg
    -> m (Dynamic t Text)
renderMsg p master msg = renderMsgDyn p master (constDyn msg)