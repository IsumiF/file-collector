{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Handler
  (
  -- *Primary Handler
    handler
  -- *Utilities
  , toHandler
  , makeAuthCheck
  ) where

import Data.Proxy (Proxy (..))
import Servant.API
import Servant.Server

import           FileCollector.Backend.App (App)
import           FileCollector.Backend.Handler.AppHandler
    (AppHandler, toHandler)
import           FileCollector.Backend.Handler.Auth (makeAuthCheck)
import qualified FileCollector.Backend.Handler.Impl.File as File
import qualified FileCollector.Backend.Handler.Impl.User as User
import           FileCollector.Backend.Oss.Class.MonadOssService
import           FileCollector.Common.Api (Api)

-- |Handler to 'Api'
handler :: MonadOssService ossProvider App
        => Proxy ossProvider
        -> ServerT (Api ossProvider) AppHandler
handler p = File.handler p :<|> User.handler
