module FileCollector.Backend.Handler
  ( handler
  , toHandler
  , makeAuthCheck
  ) where

import Servant.Server

import           FileCollector.Backend.Handler.AppHandler
    (AppHandler, toHandler)
import           FileCollector.Backend.Handler.Auth (makeAuthCheck)
import qualified FileCollector.Backend.Handler.Impl.User as User
import           FileCollector.Common.Api (Api)

handler :: ServerT Api AppHandler
handler = User.handler
