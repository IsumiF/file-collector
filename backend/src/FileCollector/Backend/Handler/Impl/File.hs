module FileCollector.Backend.Handler.Impl.File
  ( handler
  ) where

import Servant

import qualified FileCollector.Backend.Core.File as Core
import           FileCollector.Backend.Handler.AppHandler
import           FileCollector.Common.Api.Auth
    (UserCollector (..), UserUploader (..))
import           FileCollector.Common.Api.File

handler :: ServerT (Api ossProvider) AppHandler
handler =
    handlerDir
  :<|> undefined

handlerDir :: ServerT (ApiDir ossProvider) AppHandler
handlerDir =
    handlerGetDirList
  :<|> handlerGetDir
  :<|> handlerPutDir
  :<|> undefined

handlerGetDirList :: ServerT ApiGetDirList AppHandler
handlerGetDirList (UserUploader user) =
    lift $ Core.getVisibleDirectories user

handlerGetDir :: ServerT ApiGetDir AppHandler
handlerGetDir (UserUploader user) ownerName dirName = do
    maybeDir <- lift $ Core.getDirectory user ownerName dirName
    maybe (throwError err404) pure maybeDir

handlerPutDir :: ServerT ApiPutDir AppHandler
handlerPutDir (UserCollector me) userName dirName newDir = do
    result <- lift $ Core.updateDirectory me userName dirName newDir
    case result of
      Left err ->
        case err of
          Core.UDErrNoSuchDirectory -> throwError err404
          Core.UDErrCanNotUpdate    -> throwError err403
      Right _ ->
        pure ()
