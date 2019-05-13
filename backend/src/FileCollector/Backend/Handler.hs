module FileCollector.Backend.Handler
  (
  -- *Primary Handler
    handler
  -- *Utilities
  , toHandler
  , makeAuthCheck
  ) where

import Servant.Server

import FileCollector.Backend.Handler.AppHandler (AppHandler, toHandler)
import FileCollector.Backend.Handler.Auth (makeAuthCheck)
import FileCollector.Common.Api (Api)

-- |Handler to 'Api'
handler :: ServerT (Api ossProvider) AppHandler
handler = undefined
