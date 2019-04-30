{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Frontend.Class.MonadOssClient
  ( MonadOssClient(..)
  ) where

import           JSDOM.Types                            (File)

import           FileCollector.Common.Types.OssProvider

class (OssProvider provider, Monad m) => MonadOssClient provider m where
  uploadTo :: Credential provider -- ^credential
           -> File -- ^local source
           -> FileLocation provider -- ^remote destination
           -> m ()
  downloadFrom :: Credential provider -- ^credential
               -> FileLocation provider -- ^file to download
               -> m ()
