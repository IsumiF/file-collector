module FileCollector.Backend.Handler.Impl.User
  ( handler
  ) where

import Servant.Server

import qualified FileCollector.Backend.Core.User as Core
import           FileCollector.Backend.Handler.AppHandler
import           FileCollector.Backend.Handler.Impl.Prelude
import           FileCollector.Common.Api.Auth (UserUploader (..))
import qualified FileCollector.Common.Api.User as User

handler :: ServerT User.Api AppHandler
handler =
    -- handleCreateUser
    -- :<|> handleGetUserList
    -- :<|> handleDeleteUser
    handleGetUser
    -- :<|> handlePutUser
    -- :<|> handleChangePassword

-- handleCreateUser :: ServerT User.ApiCreateUser AppHandler
-- handleCreateUser = _

-- handleGetUserList :: ServerT User.ApiGetUserList AppHandler
-- handleGetUserList = _

-- handleDeleteUser :: ServerT User.ApiDeleteUser AppHandler
-- handleDeleteUser = _

handleGetUser :: ServerT User.ApiGetUser AppHandler
handleGetUser (UserUploader me) name = throw404OnNothing $ Core.getUser me name

-- handlePutUser :: ServerT User.ApiPutUser AppHandler
-- handlePutUser = _

-- handleChangePassword :: ServerT User.ApiChangePassword AppHandler
-- handleChangePassword = _
