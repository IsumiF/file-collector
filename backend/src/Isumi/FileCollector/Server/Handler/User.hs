module Isumi.FileCollector.Server.Handler.User
  ( server
  ) where

import Isumi.FileCollector.Api.User (Api)
import Isumi.FileCollector.Server.Persist.User
import Isumi.FileCollector.Server.Handler.Internal.Prelude

server :: UserUploader -> ServerT Api AppHandler
server (UserUploader user) = do
    let name = userName user
    runDbOp $ getRoleByName name
