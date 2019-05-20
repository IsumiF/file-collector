{-# LANGUAGE FlexibleContexts #-}

module FileCollector.Backend.Database.Impl.PendingUploadFile
  ( addPendingUploadFile
  , updatePendingUploadFile
  , isPendingUploadFileExist
  , getPendingUploadFile
  , removePendingUploadFile
  ) where

import Control.Monad.Trans.Maybe
import Data.Maybe (isJust)
import Data.Time (UTCTime)

import FileCollector.Backend.Database.Impl.Internal.Prelude

addPendingUploadFile :: MonadSqlDb m
                     => PendingUploadFile
                     -> m ()
addPendingUploadFile = liftPersist . insert_

updatePendingUploadFile :: MonadSqlDb m
                        => Unique PendingUploadFile
                        -> Maybe Text
                        -> UTCTime
                        -> m ()
updatePendingUploadFile uniqueKey newName newTime =
    liftPersist $ void $ runMaybeT $ do
      en <- MaybeT $ getBy uniqueKey
      let enId = entityKey en
      lift $ update enId
        [ PendingUploadFileNewName =. newName
        , PendingUploadFileRequestTime =. newTime
        ]

isPendingUploadFileExist :: MonadSqlDb m
                         => Unique PendingUploadFile
                         -> m Bool
isPendingUploadFileExist uniqueKey =
    fmap isJust (getPendingUploadFile uniqueKey)

getPendingUploadFile :: MonadSqlDb m
                     => Unique PendingUploadFile
                     -> m (Maybe PendingUploadFile)
getPendingUploadFile uniqueKey = liftPersist $ runMaybeT $ do
    en <- MaybeT $ getBy uniqueKey
    pure $ entityVal en

removePendingUploadFile :: MonadSqlDb m
                        => Unique PendingUploadFile
                        -> m ()
removePendingUploadFile = liftPersist . deleteBy
