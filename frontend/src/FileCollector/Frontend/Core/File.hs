{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module FileCollector.Frontend.Core.File
  ( downloadFile
  , downloadFileFromUrl
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Functor (void)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.HTMLLinkElement as DOM
import qualified GHCJS.DOM.Types as DOM
import           Reflex.Dom
import           Servant.Reflex

import           Control.Lens
import           FileCollector.Common.Types
import qualified FileCollector.Frontend.Class.Service.MonadFile as Service

downloadFile ::
  ( MonadWidget t m
  , Service.MonadFile t m
  ) => FullFilePath
    -> Event t a -- ^trigger event
    -> m ()
downloadFile fullFilePath triggerEvt = do
    reqResultEvt <- Service.getFile
      (constDyn . Right $ fullFilePath ^. fullFilePath_dirOwnerName)
      (constDyn . Right $ fullFilePath ^. fullFilePath_dirName)
      (constDyn . Right $ fullFilePath ^. fullFilePath_uploaderName)
      (constDyn . Right $ fileName)
      (constDyn True)
      (void triggerEvt)
    let urlEvt = fmapMaybe id $ ffor reqResultEvt $ \case
          ResponseSuccess _ (_, Just (AliyunSignedUrl url)) _ -> Just url
          _ -> Nothing

    void $ downloadFileFromUrl ((fileName,) <$> urlEvt)
  where
    fileName = fullFilePath ^. fullFilePath_fileName

downloadFileFromUrl ::
  ( MonadWidget t m
  ) => Event t (FileName, Text)
    -> m (Event t ())
downloadFileFromUrl triggerEvt = do
    void $ performEvent $ ffor triggerEvt $ \(FileName fileName, url) -> do
      Just doc <- DOM.currentDocument
      a <- DOM.uncheckedCastTo DOM.HTMLLinkElement <$> DOM.createElement doc ("a" :: Text)
      DOM.setHref a url
      DOM.setAttribute a ("download" :: Text) fileName
      DOM.click a
    pure never
