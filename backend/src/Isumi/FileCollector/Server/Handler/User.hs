module Isumi.FileCollector.Server.Handler.User
  ( server
  ) where

import Isumi.FileCollector.Api.User
import Isumi.FileCollector.Server.Auth (UserUploader (..))
import Isumi.FileCollector.Server.Handler.Prelude
import Isumi.FileCollector.Server.Persist.Entity (User (..))
import Isumi.FileCollector.Server.Persist.User

server :: UserUploader -> ServerT Api AppHandler
server (UserUploader user) = do
    let name = userName user
    runDbOp $ getRoleByName name
