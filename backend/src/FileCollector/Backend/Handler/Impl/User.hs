{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Backend.Handler.Impl.User
  ( handlerGet
  ) where

import Control.Monad.Trans.Class (lift)
import Data.Maybe (maybe)
import Servant.Server

import           FileCollector.Backend.Core.User (getUserByName)
import           FileCollector.Backend.Handler.AppHandler
import qualified FileCollector.Common.Api.User as User

handlerGet :: ServerT User.Api AppHandler
handlerGet maybeName = do
    let name =
          case maybeName of
            Nothing    -> "" -- TODO use current authenticated user
            Just name' -> name'
    maybeUser <- lift $ getUserByName name
    maybe (throwError err404) pure maybeUser
