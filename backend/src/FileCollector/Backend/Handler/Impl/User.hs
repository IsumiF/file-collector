module FileCollector.Backend.Handler.Impl.User
  ( handler
  ) where

import Control.Lens
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe, maybe)
import Servant.Server

import           FileCollector.Backend.Core.User (getUserByName)
import           FileCollector.Backend.Handler.AppHandler
import           FileCollector.Common.Api.Auth (UserUploader (..))
import qualified FileCollector.Common.Api.User as User
import           FileCollector.Common.Types.User

handler :: ServerT User.Api AppHandler
handler (UserUploader me) maybeName = do
    let name = fromMaybe (me ^. user_name) maybeName
    maybeUser <- lift $ getUserByName name
    maybe (throwError err404) pure maybeUser
