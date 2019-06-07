module FileCollector.Frontend.Class.BaseUrl
  ( HasBaseUrl(..)
  ) where

import Servant.Reflex (BaseUrl)

class HasBaseUrl a where
  getBaseUrl :: a -> BaseUrl

instance HasBaseUrl BaseUrl where
  getBaseUrl = id
