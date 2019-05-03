module FileCollector.Backend.Handler.Impl.User
  ( handler
  ) where

import Control.Monad.Trans.Class (lift)
import Data.Maybe (maybe)
import Servant.Server

import           FileCollector.Backend.Core.User (getUserByName)
import           FileCollector.Backend.Handler.AppHandler
import qualified FileCollector.Common.Api.User as User
import           FileCollector.Common.Types.User (UserName (..))

handler :: ServerT User.ApiGetUser AppHandler
-- UserUploader -> Text -> ExceptT ServantErr App (Maybe User)
handler _ (UserName name) = do
    maybeUser <- lift $ getUserByName name
    maybe (throwError err404) pure maybeUser
