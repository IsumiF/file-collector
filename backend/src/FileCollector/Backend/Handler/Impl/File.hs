module FileCollector.Backend.Handler.Impl.File
  ( handler
  ) where

import Servant

import qualified FileCollector.Backend.Core.File as Core
    (getDirectory, getVisibleDirectories)
import           FileCollector.Backend.Handler.AppHandler
import           FileCollector.Common.Api.Auth (UserUploader (..))
import           FileCollector.Common.Api.File

handler :: ServerT (Api ossProvider) AppHandler
handler =
    handlerDir
  :<|> undefined

handlerDir :: ServerT (ApiDir ossProvider) AppHandler
handlerDir =
    handlerGetDirList
  :<|> handlerGetDir
  :<|> undefined

handlerGetDirList :: ServerT ApiGetDirList AppHandler
handlerGetDirList (UserUploader user) =
    lift $ Core.getVisibleDirectories user

handlerGetDir :: ServerT ApiGetDir AppHandler
handlerGetDir (UserUploader user) ownerName dirName = do
    maybeDir <- lift $ Core.getDirectory user ownerName dirName
    maybe (throwError err404) pure maybeDir
