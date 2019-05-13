{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Handler.Impl.File
  ( handler
  ) where

import Servant

import qualified FileCollector.Backend.Core.File as Core
import           FileCollector.Backend.Handler.AppHandler
import           FileCollector.Backend.Oss.Class.MonadOssService
import           FileCollector.Common.Api.Auth
    (UserCollector (..), UserUploader (..))
import           FileCollector.Common.Api.File
import qualified FileCollector.Common.Types as Common

handler :: MonadOssService ossProvider App
        => Proxy ossProvider
        -> ServerT (Api ossProvider) AppHandler
handler ossProvider =
    handlerDir ossProvider
  :<|> undefined

handlerDir :: MonadOssService ossProvider App
           => Proxy ossProvider
           -> ServerT (ApiDir ossProvider) AppHandler
handlerDir ossProvider =
    handlerGetDirList
  :<|> handlerGetDir
  :<|> handlerPutDir
  :<|> handlerDeleteDir ossProvider
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

handlerDeleteDir :: MonadOssService provider App
                 => Proxy provider
                 -> ServerT ApiDeleteDir AppHandler
handlerDeleteDir ossProvider (UserCollector me) ownerName dirName = do
    status <- lift $ Core.deleteDirectory ossProvider me ownerName dirName
    case status of
      Right _ -> pure Common.DdrFullyDeleted
      Left err ->
        case err of
          Core.DdeNoSuchDirectory -> throwError err404
          Core.DdePartiallyDeleted -> pure Common.DdrPartiallyDeleted
