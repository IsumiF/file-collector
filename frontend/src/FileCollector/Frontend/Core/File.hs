{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module FileCollector.Frontend.Core.File
  ( downloadFile
  , downloadFileFromUrl
  , getDomFileNames
  , uploadDomFile
  , showUploadRule 
  ) where

import           Control.Monad.Reader
import           Data.Functor (void)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word32)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Blob as DomBlob
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.File as DOMFile
import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.HTMLLinkElement as DOM
import qualified GHCJS.DOM.Types as DOM
import           Reflex.Dom
import           Servant.Reflex
import           Text.Printf (printf)

import           Control.Lens
import           FileCollector.Common.Types
import           FileCollector.Frontend.Class.Language
import qualified FileCollector.Frontend.Class.Service.MonadFile as Service
import           FileCollector.Frontend.Core.Base (traverseDyn)
import           FileCollector.Frontend.Message.FileExplorer

downloadFile ::
  ( MonadWidget t m
  , Service.MonadFile t m
  ) => FullFilePath
    -> Event t a -- ^trigger event
    -> m ()
downloadFile fullFilePath triggerEvt = do
    reqResultEvt <- supplyFullFilePath (constDyn $ Right fullFilePath) Service.getFile
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
    -> m ()
downloadFileFromUrl triggerEvt =
    void $ performEvent $ ffor triggerEvt $ \(FileName fileName, url) -> do
      Just doc <- DOM.currentDocument
      a <- DOM.uncheckedCastTo DOM.HTMLLinkElement <$> DOM.createElement doc ("a" :: Text)
      DOM.setHref a url
      DOM.setAttribute a ("download" :: Text) fileName
      DOM.click a

getDomFileNames :: MonadWidget t m
                => Dynamic t [DOM.File]
                -> m (Dynamic t [Text])
getDomFileNames domFilesDyn = do
    resultEvt <- dyn $ ffor domFilesDyn $ traverse DOMFile.getName
    holdDyn [] resultEvt

{-| 上传文件
-}
uploadDomFile ::
  ( MonadWidget t m
  , Service.MonadFile t m
  ) => Event t FullFilePath
    -> Dynamic t (Maybe DOM.File)
    -> m (Event t Bool)
uploadDomFile filePathEvt domFileMaybeDyn = do
    fullFilePathDyn <- holdDyn (Left "") (fmap Right filePathEvt)

    nameEvt <- dyn $ fmap (traverse DOMFile.getName) domFileMaybeDyn
    nameDyn <- holdDyn (Nothing :: Maybe Text) nameEvt

    let nameQParamDyn = ffor nameDyn $ \nameMaybe ->
          case nameMaybe of
            Just name -> QParamSome . FileName $ name
            Nothing   -> QParamInvalid ""

    putResultEvt <- supplyFullFilePath fullFilePathDyn Service.putFile
      nameQParamDyn (void filePathEvt)

    let uploadReqEvt = ffor (attachPromptlyDyn domFileMaybeDyn putResultEvt) $ \(domFileMaybe, reqResult) ->
          let credMaybe = reqResultToMaybe reqResult
           in case (credMaybe, domFileMaybe) of
                (Just (AliyunSignedUrl putUrl), Just domFile) -> do
                  let req = xhrRequest "PUT" putUrl $ XhrRequestConfig
                        ("Content-Type" =: "application/octet-stream")
                        Nothing
                        Nothing
                        (Just XhrResponseType_Default)
                        (DomBlob.toBlob domFile)
                        False
                        AllHeaders
                  Just req
                _ -> Nothing
    uploadRespEvt <- performRequestsAsync uploadReqEvt
    let uploadStatusEvt = fmap (\resp -> resp ^. xhrResponse_status == 200)
          (fmapMaybe id uploadRespEvt)
        uploadFailedEvt = fmap (const False) $ ffilter not uploadStatusEvt
    commitResultEvt <- supplyFullFilePath fullFilePathDyn Service.commitPutFile
      (void $ ffilter id uploadStatusEvt)
    pure $ leftmost
      [ fmap isResponseSuccess commitResultEvt
      , uploadFailedEvt
      ]

supplyFullFilePath :: Reflex t
                   => Dynamic t (Either Text FullFilePath)
                   -> ( Dynamic t (Either Text UserName)
                     -> Dynamic t (Either Text DirectoryName)
                     -> Dynamic t (Either Text UserName)
                     -> Dynamic t (Either Text FileName)
                     -> a
                      )
                   -> a
supplyFullFilePath fpDyn f =
    f ((fmap . fmap) (^. fullFilePath_dirOwnerName) fpDyn)
      ((fmap . fmap) (^. fullFilePath_dirName) fpDyn)
      ((fmap . fmap) (^. fullFilePath_uploaderName) fpDyn)
      ((fmap . fmap) (^. fullFilePath_fileName) fpDyn)

reqResultToMaybe :: ReqResult tag a -> Maybe a
reqResultToMaybe (ResponseSuccess _ x _) = Just x
reqResultToMaybe _                       = Nothing

isResponseSuccess :: ReqResult tag a -> Bool
isResponseSuccess ResponseSuccess{} = True
isResponseSuccess _                 = False

showUploadRule ::
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  ) => Dynamic t UploadRule
    -> m (Dynamic t Text)
showUploadRule ruleDyn =
    traverseDyn (constDyn "") ruleDyn $ \rule ->
      case rule of
        RuleMaxFileSize maxFileSize ->
          renderMsg' (MsgMaxFileSize (fileSizeInMB maxFileSize))
        RuleFileNameFormat nameFormat ->
          renderMsg' (MsgFileNameFormat nameFormat)
        RuleMaxFiles maxFiles ->
          renderMsg' (MsgMaxFiles (T.pack . show $ maxFiles))
  where
    pEnv = Proxy :: Proxy env
    renderMsg' = renderMsg pEnv FileExplorer

fileSizeInMB :: Word32 -> Text
fileSizeInMB x = T.pack . printf "%.2f" $ y
  where
    y = fromIntegral x / (1024 * 1024) :: Double
