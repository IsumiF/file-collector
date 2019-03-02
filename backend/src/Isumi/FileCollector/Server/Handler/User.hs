module Isumi.FileCollector.Server.Handler.User
  ( server
  ) where

import Isumi.FileCollector.Api.User (Api)
import Isumi.FileCollector.Server.Handler.Internal.Prelude
import Isumi.FileCollector.Server.Persist.User

server :: UserUploader -> ServerT Api AppHandler
server (UserUploader user) = do
    let name = userName user
    runDbOp $ getRoleByName name
