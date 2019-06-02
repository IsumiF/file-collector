{-# LANGUAGE FlexibleInstances #-}

module FileCollector.Backend.Database.Class.MonadPendingUploadFile
  ( MonadPendingUploadFile (..)
  ) where

import Data.Time (UTCTime)

import           FileCollector.Backend.Database.Class.Internal.Prelude
import qualified FileCollector.Backend.Database.Impl.PendingUploadFile as Impl

class Monad m => MonadPendingUploadFile m where
  -- |添加一个等待上传项
  addPendingUploadFile :: PendingUploadFile -> m ()
  -- |更新等待上传项。只有新名称和请求时间两个属性可以更新
  updatePendingUploadFile :: Unique PendingUploadFile
                          -> Maybe Text
                          -> UTCTime
                          -> m ()
  -- |检查一个等待上传项是否存在
  isPendingUploadFileExist :: Unique PendingUploadFile
                           -> m Bool
  getPendingUploadFile :: Unique PendingUploadFile -> m (Maybe PendingUploadFile)
  -- |删除一个等待上传项。如果这项不存在，就什么都不做
  removePendingUploadFile :: Unique PendingUploadFile -> m ()
  removeAllPendingUploadFilesOfDirectory :: DirectoryId -> m ()

instance MonadIO m => MonadPendingUploadFile (ReaderT SqlBackend m) where
  addPendingUploadFile = Impl.addPendingUploadFile
  updatePendingUploadFile = Impl.updatePendingUploadFile
  isPendingUploadFileExist = Impl.isPendingUploadFileExist
  getPendingUploadFile = Impl.getPendingUploadFile
  removePendingUploadFile = Impl.removePendingUploadFile
  removeAllPendingUploadFilesOfDirectory = Impl.removeAllPendingUploadFilesOfDirectory

instance (MonadPendingUploadFile m) => MonadPendingUploadFile (MaybeT m) where
  addPendingUploadFile = lift . addPendingUploadFile
  updatePendingUploadFile a b c = lift $ updatePendingUploadFile a b c
  isPendingUploadFileExist = lift . isPendingUploadFileExist
  getPendingUploadFile = lift . getPendingUploadFile
  removePendingUploadFile = lift . removePendingUploadFile
  removeAllPendingUploadFilesOfDirectory = lift . removeAllPendingUploadFilesOfDirectory

instance (MonadPendingUploadFile m) => MonadPendingUploadFile (ExceptT e m) where
  addPendingUploadFile = lift . addPendingUploadFile
  updatePendingUploadFile a b c = lift $ updatePendingUploadFile a b c
  isPendingUploadFileExist = lift . isPendingUploadFileExist
  getPendingUploadFile = lift . getPendingUploadFile
  removePendingUploadFile = lift . removePendingUploadFile
  removeAllPendingUploadFilesOfDirectory = lift . removeAllPendingUploadFilesOfDirectory
