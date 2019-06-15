module FileCollector.Frontend.Core.Base
  ( traverseDyn
  , reqResultToMaybe
  , isResponseSuccess
  ) where

import Control.Monad (join)
import Reflex.Dom
import Servant.Reflex

traverseDyn :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => Dynamic t b
            -> Dynamic t a
            -> (a -> m (Dynamic t b))
            -> m (Dynamic t b)
traverseDyn defY x f = do
    dynEvt <- dyn $ fmap f x
    dynDyn <- holdDyn defY dynEvt
    pure $ join dynDyn

reqResultToMaybe :: ReqResult tag a -> Maybe a
reqResultToMaybe (ResponseSuccess _ x _) = Just x
reqResultToMaybe _                       = Nothing

isResponseSuccess :: ReqResult tag a -> Bool
isResponseSuccess ResponseSuccess{} = True
isResponseSuccess _                 = False
