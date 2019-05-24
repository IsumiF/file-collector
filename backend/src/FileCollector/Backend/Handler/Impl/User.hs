module FileCollector.Backend.Handler.Impl.User
  ( handler
  ) where

import Servant.Server

import           FileCollector.Backend.Handler.AppHandler
import qualified FileCollector.Common.Api.User as User

handler :: ServerT User.Api AppHandler
-- UserUploader -> Text -> ExceptT ServantErr App (Maybe User)
handler = _
    -- maybeUser <- lift $ getUserByName name
    -- maybe (throwError err404) pure maybeUser
