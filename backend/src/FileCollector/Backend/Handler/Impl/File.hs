module FileCollector.Backend.Handler.Impl.File
  ( handler
  ) where

import Servant

import           FileCollector.Backend.App (App)
import qualified FileCollector.Backend.Core.File as Core (getVisibleDirectories)
import           FileCollector.Common.Api.Auth (UserUploader (..))
import           FileCollector.Common.Api.File

handler :: ServerT (Api ossProvider) App
handler =
    handlerDir
  :<|> undefined

handlerDir :: ServerT (ApiDir ossProvider) App
handlerDir =
    handlerGetDirList
  :<|> undefined

handlerGetDirList :: ServerT ApiGetDirList App
handlerGetDirList (UserUploader user) =
    Core.getVisibleDirectories user
