module FileCollector.Backend.Core.File.Authorization
  ( withFileAuthorized
  , OnFileAuthorized
  ) where

import Control.Lens
import Control.Monad.Trans.Class (lift)

import           Control.Monad.Trans.Maybe
import qualified FileCollector.Backend.Database.Class as Db
import qualified FileCollector.Backend.Database.Types as Db
import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types

withFileAuthorized ::
  ( Db.MonadReadFile m
  , Db.MonadReadDirectory m
  , Db.MonadReadUser m
  , Db.MonadDirectoryUploader m
  )
  => User
  -> FullFilePath
  -> OnFileAuthorized m a
  -> MaybeT m a
withFileAuthorized me fullPath action = do
    myId <- MaybeT $ Db.getIdByUserName (convert $ me ^. user_name)
    uploaderId <- MaybeT $ Db.getIdByUserName uploaderName
    dirId <- MaybeT $ Db.getDirectoryId ownerName dirName
    fileId <- Db.getFileId ownerName dirName uploaderName fileName
    authorized <-
      case me ^. user_role of
        RoleAdmin -> pure True
        RoleCollector -> pure $ me ^. user_name == convert ownerName
        RoleUploader ->
          if me ^. user_name /= convert uploaderName
          then pure False
          else Db.dirHasUploader dirId uploaderId
    if not authorized
    then MaybeT $ pure Nothing
    else lift $ action myId uploaderId dirId fileId
  where
    ownerName = convert $ fullPath ^. fullFilePath_dirOwnerName
    dirName = convert $ fullPath ^. fullFilePath_dirName
    uploaderName = convert $ fullPath ^. fullFilePath_uploaderName
    fileName = convert $ fullPath ^. fullFilePath_fileName

type OnFileAuthorized m a =
     Db.UserId -- ^current user id
  -> Db.UserId -- ^uploader id
  -> Db.DirectoryId -- ^directory id
  -> Maybe Db.FileId -- ^file id
  -> m a
