{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.UI.FileExplorer
  ( widget
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Foldable (sequenceA_)
import           Data.Maybe (listToMaybe)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.File as DOMFile
import qualified GHCJS.DOM.Types as DOM (File)
import           Reflex.Dom
import           Servant.Reflex (ReqResult (..))

import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types
import           FileCollector.Frontend.Class.Language
import           FileCollector.Frontend.Class.MonadTimeZone
import qualified FileCollector.Frontend.Class.Service.MonadDirectory as Service
import qualified FileCollector.Frontend.Class.Service.MonadDirectoryContent as Service
import qualified FileCollector.Frontend.Class.Service.MonadDirectoryUploaders as Service
import qualified FileCollector.Frontend.Class.Service.MonadFile as Service
import           FileCollector.Frontend.Class.User
import           FileCollector.Frontend.Core.Base
import qualified FileCollector.Frontend.Core.File as Core
import           FileCollector.Frontend.Core.TimeZone (showUTCTime)
import           FileCollector.Frontend.Message.FileExplorer
import           FileCollector.Frontend.UI.Component.Button
import           FileCollector.Frontend.UI.Component.Dialog
import           FileCollector.Frontend.UI.FileExplorer.DirectorySettings
    (directorySettings)

widget ::
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , HasUser t env
  , MonadTimeZone t m
  , Service.MonadDirectory t m
  , Service.MonadDirectoryContent t m
  , Service.MonadFile t m
  , Service.MonadDirectoryUploaders t m
  ) => m ()
widget = showIfLoggedIn $ mdo
    viewingDirEvt' <- dyn $ ffor viewingDirDyn $ \case
      Nothing -> do
        user <- ask
        let isCollector = user ^. user_role >= RoleCollector
        (fmap . fmap) Just (lift (directoryWidget isCollector))
      Just viewingDir ->  (fmap . fmap) (const Nothing) (fileWidget viewingDir)
    viewingDirEvt <- switchHold never viewingDirEvt'
    viewingDirDyn <- holdDyn Nothing viewingDirEvt

    blank

showIfLoggedIn ::
  ( MonadWidget t m
  , MonadReader env m
  , HasUser t env
  ) => ReaderT User m ()
    -> m ()
showIfLoggedIn sub = do
    userDyn <- asks getUser
    dyn_ $ ffor userDyn $ \userMaybe ->
      case userMaybe of
        Just user -> runReaderT sub user
        Nothing   -> blank

-- |File list in a directory
fileWidget ::
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , HasUser t env
  , Service.MonadDirectoryContent t m
  , Service.MonadFile t m
  , MonadTimeZone t m
  ) => Directory
    -> ReaderT User m (Event t ()) -- ^on click back
fileWidget dir =
    elClass "nav" "panel" $ mdo
      elClass "p" "FileExplorer_heading" $ text (convert dirName)
      (backEvt, clickRefresh, uploadSelectedEvt, selectedFilesDyn) <-
        divClass "panel-block" $
          elAttr "div" ("id" =: "FileExplorer_toolBar") $
            (,,,) <$> fmap fst (buttonAttr ("id" =: "FileExplorer_fileBackButton") $
                        dynText msgBack)
                  <*> fmap fst (buttonClassAttr "button is-primary" mempty $
                        dynText msgRefresh)
                  <*> fmap fst (buttonClassAttr "button" mempty $
                        dynText msgUploadSelected)
                  <*> fileChooser msgChooseFile
      divClass "panel-block" $ -- show ddl
        dynText ddlTextDyn
      divClass "panel-block" $ do -- show upload rules
        let rules = dir ^. directory_uploadRules
        shownRules <- sequenceA <$> traverse (lift . Core.showUploadRule) (fmap constDyn rules)
        let rulesTxt = fmap (T.intercalate ", ") shownRules
        dynText $ msgUploadRules <> rulesTxt
      anyUnselectEvtEvt <- divClass "panel-block" $
        elClass "table" "FileExplorer_table" $ mdo
          colsFillAt 2 5
          cb <- el "thead" $
            el "tr" $ do
              el "th" $ dynText msgFileName
              el "th" $ dynText msgUploaderName
              el "th" $ dynText msgLastModificationTime
              el "th" $ dynText msgDelete
              el "th" $
                elClass "label" "checkbox" $ do
                  dynText msgSelectAll
                  checkbox False cbConfig
          r <- el "tbody" $
            dyn $ ffor fileListDyn $ \files -> do
              xs <- traverse (lift . fileRow selectAllEvt dir) files
              pure (leftmost (fmap fst xs))

          let selectAllEvt = void $ ffilter id (_checkbox_change cb)
          pure r

      let refreshEvt = leftmost [postBuildEvt, clickRefresh, void uploadFileResult]
      fileListReqResultEvt <- lift $ Service.getDirContent
        (constDyn . Right $ dir ^. directory_ownerName)
        (constDyn . Right $ dir ^. directory_name)
        refreshEvt
      let fileListEvt = ffor fileListReqResultEvt $ \case
            ResponseSuccess _ xs _ -> xs
            _ -> []
      fileListDyn <- holdDyn [] fileListEvt

      anyUnselectEvt <- switchHold never anyUnselectEvtEvt
      let cbConfig = CheckboxConfig (False <$ anyUnselectEvt) (constDyn mempty)

      -- Upload new file
      user <- ask
      uploadFileResult <- lift $ uploadFile uploadSelectedEvt selectedFilesDyn $
        FullFilePath (dir ^. directory_ownerName)
                     (dir ^. directory_name)
                     (user ^. user_name)
      uploadFileResultDyn <- holdDyn False uploadFileResult
      let msgUploadResultDyn = do
            r <- uploadFileResultDyn
            if r then msgUploadSucceeded else msgUploadFailed
      popupMessage DialogStyleSuccess (ffilter id uploadFileResult) msgUploadResultDyn
      popupMessage DialogStyleWarning (ffilter not uploadFileResult) msgUploadResultDyn
      -- DDL Text
      ddlTextDyn <- case dir ^. directory_expirationTime of
        Nothing -> pure msgNoDeadline
        Just ddl -> do
          ddlText <- lift $ showUTCTime (constDyn ddl)
          traverseDyn (constDyn "") ddlText msgDeadlineIs

      -- Upload rules


      -- Messages
      msgFileName <- renderMsg' MsgFileName
      msgUploaderName <- renderMsg' MsgUploaderName
      msgLastModificationTime <- renderMsg' MsgLastModificationTime
      msgBack <- renderMsg' MsgBack
      msgRefresh <- renderMsg' MsgRefresh
      msgSelectAll <- renderMsg' MsgSelectAll
      msgUploadSelected <- renderMsg' MsgUploadSelected
      msgChooseFile <- renderMsg' MsgChooseFile
      msgUploadSucceeded <- renderMsg' MsgUploadSucceeded
      msgUploadFailed <- renderMsg' MsgUploadFailed
      msgNoDeadline <- renderMsg' MsgNoDeadline
      let msgDeadlineIs ddl = renderMsg' (MsgDeadlineIs ddl)
      msgUploadRules <- renderMsg' MsgUploadRules
      msgDelete <- renderMsg' MsgDelete
      --
      postBuildEvt <- getPostBuild
      --
      pure backEvt
  where
    dirName = dir ^. directory_name
    renderMsg' = lift . renderMsg (Proxy :: Proxy env) FileExplorer

uploadFile ::
  ( MonadWidget t m
  , Service.MonadFile t m
  ) => Event t () -- ^trigger
    -> Dynamic t [DOM.File] -- ^selected files (we only use the first for now)
    -> (FileName -> FullFilePath)
    -> m (Event t Bool)
uploadFile triggerEvt filesDyn nameToFullPath = do
    let fileDyn = fmap listToMaybe filesDyn
        triggerEvtWithFile = fmapMaybe listToMaybe (tagPromptlyDyn filesDyn triggerEvt)

    fullPathEvt <- performEvent $ ffor triggerEvtWithFile $ \domFile -> do
      (domFileName :: Text) <- DOMFile.getName domFile
      pure $ nameToFullPath (FileName domFileName)

    Core.uploadDomFile fullPathEvt fileDyn

fileRow ::
  ( MonadWidget t m
  , Service.MonadFile t m
  , MonadTimeZone t m
  ) => Event t () -- ^select all
    -> Directory -- ^the directory that contains the file
    -> File
    -> m (Event t (), Event t ()) -- ^unselect current row
fileRow selectAllEvt dir file =
    el "tr" $ mdo
      (clickFileNameEvt, _) <- el "td" $ buttonAttr ("class" =: "button is-outlined") $
        text (convert $ file ^. file_name)
      void $ el "td" $ buttonAttr ("class" =: "button is-outlined") $
        text (convert $ file ^. file_uploaderName)
      void $ el "td" $ do
        timeText <- showUTCTime . constDyn $ file ^. file_lastModified
        dynText timeText
      deleteEvt <- el "td" $ do
        (e, _) <- buttonClassAttr "button" ("type" =: "button") $
          elClass "span" "icon" $
            elClass "i" "fas fa-times" blank
        pure e
      cb <- el "td" $
        checkbox False cbConf

      let cbConf = CheckboxConfig (fmap (const True) selectAllEvt) (constDyn mempty)
      let fullFilePath = FullFilePath
            (dir ^. directory_ownerName)
            (dir ^. directory_name)
            (file ^. file_uploaderName)
            (file ^. file_name)

      Core.downloadFile fullFilePath clickFileNameEvt

      void $ Core.supplyFullFilePath (constDyn (Right fullFilePath)) Service.deleteFile deleteEvt

      pure (void (ffilter not (_checkbox_change cb)), never)
      -- pure (void (ffilter not (_checkbox_change cb)), void deleteFileReqResult)

fileChooser :: MonadWidget t m
            => Dynamic t Text
            -> m (Dynamic t [DOM.File])
fileChooser msgChooseFile =
    divClass "file" $
      elClass "label" "file-label" $ do
        fi <- fileInput (FileInputConfig . constDyn $ "class" =: "file-input")
        fileNamesDyn <- Core.getDomFileNames (value fi)
        elClass "span" "file-cta" $ do
          elClass "span" "file-icon" $
            elClass "i" "fas fa-upload" blank
          elClass "span" "file-label" $ dynText msgChooseFile
        elClass "span" "file-name" $ dynText (fmap (headOrDefault "") fileNamesDyn)
        pure $ value fi

headOrDefault :: a -> [a] -> a
headOrDefault y []     = y
headOrDefault _ (x: _) = x

colsFillAt :: DomBuilder t m
           => Int -- ^index of col to set to 'width: fill'
           -> Int -- ^total number of cols
           -> m ()
colsFillAt i n =
    el "colgroup" $ do
      sequenceA_ $ replicate i colFitContent
      colFill
      sequenceA_ $ replicate (n - i - 1) colFitContent
  where
    colFitContent = elClass "col" "FileExplorer_colFitContent" blank
    colFill = elClass "col" "FileExplorer_colFill" blank

-- |Directory list
directoryWidget ::
  ( MonadWidget t m
  , Service.MonadDirectory t m
  , Service.MonadFile t m
  , MonadReader env m
  , HasLanguage t env
  , MonadTimeZone t m
  , Service.MonadDirectoryUploaders t m
  ) => Bool
    -> m (Event t Directory) -- ^enter specific directory
directoryWidget isCollector = mdo
    enterDirEvt <- elClass "nav" "panel" $ do
      elClass "p" "FileExplorer_heading" $ dynText dirListHeaderDyn
      when isCollector $
        divClass "panel-block" $ do
          void $ buttonClassAttr "button" ("type" =: "button") $
            dynText msgDownloadSelected
          void $ buttonClassAttr "button" ("type" =: "button") $
            dynText msgDeleteSelected
          void $ buttonClassAttr "button" ("type" =: "button") $
            dynText msgAddDirectory
          blank
      divClass "panel-block" $
        elClass "table" "FileExplorer_table" $ mdo
          colsFillAt 1 4
          selectAllCb <- el "thead" $
            el "tr" $ do
              el "th" $ dynText msgDirNameDyn
              el "th" $ dynText msgDirOwnerDyn
              when isCollector $ el "th" $ dynText msgSettings
              el "th" $
                elClass "label" "checkbox" $ do
                  dynText msgSelectAllDyn
                  checkbox False selectAllConfig
          (anyUnselectEvt, enterDirEvt') <- el "tbody" $ do
            dirRowsEvt <- dyn $ ffor dirListDyn $ \dirs ->
              traverse (dirRow isCollector (selectAllCb ^. checkbox_change)) dirs
            let anyUnselectEvt' = fmap (leftmost . fmap _dirRow_unSelect) dirRowsEvt
                enterDirEvt'' = fmap (leftmost . fmap _dirRow_enterDir) dirRowsEvt
                -- isSelectedDynEvt = fmap (traverse _dirRow_isSelected) dirRowsEvt
            -- isSelectedDyn'' <- join <$> foldDyn const (constDyn []) isSelectedDynEvt
            (,) <$> switchHold never anyUnselectEvt'
                 <*> switchHold never enterDirEvt''

          let selectAllConfig = CheckboxConfig (fmap (const False) anyUnselectEvt) (constDyn mempty)

          pure enterDirEvt'

    dirListHeaderDyn <- renderMsg' MsgDirectoryListHeader
    msgSelectAllDyn <- renderMsg' MsgSelectAll
    msgDirNameDyn <- renderMsg' MsgDirName
    msgDirOwnerDyn <- renderMsg' MsgDirOwner
    msgSettings <- renderMsg' MsgSettings
    msgDownloadSelected <- renderMsg' MsgDownloadSelected
    msgDeleteSelected <- renderMsg' MsgDeleteSelected
    msgAddDirectory <- renderMsg' MsgAddDirectory

    postBuildEvt <- getPostBuild
    dirListResultEvt <- Service.getDirList postBuildEvt
    let dirListEvt = ffor dirListResultEvt $ \case
          ResponseSuccess _ xs _ -> xs
          _ -> []
    dirListDyn <- holdDyn [] dirListEvt

    pure enterDirEvt

  where
    pEnv = Proxy :: Proxy env
    renderMsg' = renderMsg pEnv FileExplorer

dirRow ::
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , MonadTimeZone t m
  , Service.MonadDirectoryUploaders t m
  , Service.MonadDirectory t m
  ) => Bool -- ^is collector
    -> Event t Bool -- ^select all
    -> Directory
    -> m (DirRow t) -- ^on uncheck 'select' checkbox
dirRow isCollector selectAllChangeEvt dir =
    el "tr" $ mdo
      (enterDirEvt, _) <- el "td" $ buttonAttr ("class" =: "FileExplorer_dirButton") $
        text (convert dirName)
      _ <- el "td" $ buttonAttr ("class" =: "FileExplorer_dirOwnerButton") $
        text (convert $ dir ^. directory_ownerName)
      when isCollector $ el "td" $ do
        (clickEvt, _) <- buttonClassAttr "button" mempty $
          elClass "span" "icon" $
            elClass "i" "fas fa-cog" blank
        void $ directorySettings (fmap (const dir) clickEvt)
      (v, cbChangeEvt) <- elAttr "td" ("id" =: "FileExplorer_dirCheckboxCell") $
        elClass "label" "checkbox" $ do
          (Checkbox v' cbChangeEvt') <- checkbox False cbConf
          pure (v', cbChangeEvt')

      let selectAllEvt = ffilter id selectAllChangeEvt
          cbConf = CheckboxConfig selectAllEvt (constDyn mempty)
          unselectEvt = fmapMaybe (\x -> if x then Nothing else Just ()) cbChangeEvt

      pure $ DirRow unselectEvt (fmap (const dir) enterDirEvt) v
  where
    dirName = dir ^. directory_name

data DirRow t = DirRow
  { _dirRow_unSelect   :: Event t ()
  , _dirRow_enterDir   :: Event t Directory
  , _dirRow_isSelected :: Dynamic t Bool
  }

-- idAttr :: Text -> Map Text Text
-- idAttr id' = "id" =: ("FileExplorer_" <> id')
