module FileCollector.Frontend.Core.Base
  ( traverseDyn
  ) where

import Control.Monad (join)
import Reflex.Dom

traverseDyn :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => Dynamic t b
            -> Dynamic t a
            -> (a -> m (Dynamic t b))
            -> m (Dynamic t b)
traverseDyn defY x f = do
    dynEvt <- dyn $ fmap f x
    dynDyn <- holdDyn defY dynEvt
    pure $ join dynDyn
