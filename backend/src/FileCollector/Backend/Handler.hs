module FileCollector.Backend.Handler
  (
  -- *Primary Handler
    handler
  -- *Utilities
  , toHandler
  , makeAuthCheck
  ) where

import Servant.Server

import           FileCollector.Backend.Handler.AppHandler
    (AppHandler, toHandler)
import           FileCollector.Backend.Handler.Auth (makeAuthCheck)
import qualified FileCollector.Backend.Handler.Impl.User as User
import           FileCollector.Common.Api (Api)

-- |Handler to 'Api'
handler :: ServerT Api AppHandler
handler = User.handler
