{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.UI.FileExplorer.DirectorySettings
  ( directorySettings
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import           Data.Monoid
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (getCurrentTimeZone)
import           Data.Traversable (for)
import           Data.Word (Word32)
import           Generics.Deriving.Monoid
import           Generics.Deriving.Semigroup
import           GHC.Generics (Generic)
import           Reflex.Dom
import           Text.Read (readMaybe)

import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types
import           FileCollector.Frontend.Class.Language
import           FileCollector.Frontend.Class.MonadTimeZone
import qualified FileCollector.Frontend.Class.Service.MonadDirectory as Service
import qualified FileCollector.Frontend.Class.Service.MonadDirectoryUploaders as Service
import           FileCollector.Frontend.Core.Base
    (isResponseSuccess, reqResultToMaybe)
import           FileCollector.Frontend.Message.FileExplorer.DirectorySettings
import           FileCollector.Frontend.UI.Component.Button
import           FileCollector.Frontend.UI.Component.Dialog
    (DialogStyle (..), dialog, popupMessage)
import           FileCollector.Frontend.UI.Component.Util (concatM)

directorySettings ::
  ( MonadWidget t m
  , MonadReader env m
  , HasLanguage t env
  , MonadTimeZone t m
  , Service.MonadDirectoryUploaders t m
  , Service.MonadDirectory t m
  ) => Event t Directory
    -> m ()
directorySettings openEvt =
    dialog DialogStyleDefault openEvt $ mdo
      handlerParams <- elClass "nav" "panel" $ concatM
        [ elClass "p" "panel-heading" $ dynText msgTitle >> pure mempty
        , elAttr "div" ("id" =: "DirectorySettings_formContainer") $ concatM
          [ el "form" $ concatM
              [ divClass "field is-horizontal" $ do
                  divClass "field-label is-normal" $
                    elClass "label" "label" $ dynText msgName
                  divClass "field-body" $
                    divClass "control" $ do
                      ti <- textInput $ TextInputConfig "text" "" setNameEvt (constDyn $ "class" =: "input")
                      pure $ mempty { nameTi = Last . Just $ ti }
              , divClass "field is-horizontal" $ do
                  divClass "field-label is-normal" $
                    elClass "label" "label" $ dynText msgDeadline
                  divClass "field-body DirectorySettings_alignCenter" $ do
                    ddlCb <- checkbox False $ CheckboxConfig cbDeadlineSetValue (constDyn mempty)
                    ddlDateTi <- divClass "field" $ divClass "control" $
                      textInput $ TextInputConfig "date" "" setDateEvt (constDyn $ "class" =: "input")
                    ddlTimeTi <- divClass "field" $ divClass "control" $
                      textInput $ TextInputConfig "time" "" setTimeEvt (constDyn $ "class" =: "input")
                    pure $ mempty { ddlDateTi = Last (Just ddlDateTi)
                                  , ddlTimeTi = Last (Just ddlTimeTi)
                                  , ddlCb = Last (Just ddlCb)
                                  }
              , divClass "field" $ concatM
                [ elClass "label" "label" $ dynText msgUploadRules >> pure mempty
                , divClass "field is-horizontal" $ concatM
                  [ divClass "field-label" $ do
                      fileSizeLimitCb <- checkbox False $ CheckboxConfig cbFileSizeLimitSetValue (constDyn mempty)
                      dynText msgFileSizeLimit
                      pure $ mempty { fileSizeLimitCb = Last (Just fileSizeLimitCb) }
                  , divClass "field-body" $ divClass "field" $ divClass "control" $ do
                      fileSizeLimitTi <- textInput $ TextInputConfig "number" "" setFileSizeLimit (constDyn $ "class" =: "input")
                      pure $ mempty { fileSizeLimitTi = Last (Just fileSizeLimitTi) }
                  ]
                , divClass "field is-horizontal" $ do
                    fileCountLimitCb <- divClass "field-label" $
                      checkbox False (CheckboxConfig cbFileCountLimitSetValue (constDyn mempty))
                      <* dynText msgFileCountLimit
                    fileCountLimitTi <- divClass "field-body" $ divClass "field" $ divClass "control" $
                      textInput $ TextInputConfig "number" "" setFileCountLimit (constDyn $ "class" =: "input")
                    pure $ mempty { fileCountLimitCb = Last (Just fileCountLimitCb)
                                  , fileCountLimitTi = Last (Just fileCountLimitTi)
                                  }
                , divClass "field is-horizontal" $ do
                    fileNameFormatCb <- divClass "field-label" $
                      checkbox False (CheckboxConfig cbFileNameFormatSetValue (constDyn mempty))
                      <* dynText msgFileNameFormat
                    fileNameFormatTi <- divClass "field-body" $ divClass "field" $ divClass "control" $
                      textInput $ TextInputConfig "text" "" setFileNameFormat (constDyn $ "class" =: "input")
                    pure $ mempty { fileNameFormatCb = Last (Just fileNameFormatCb)
                                  , fileNameFormatTi = Last (Just fileNameFormatTi)
                                  }
                ]
              , divClass "field" $ do
                  elClass "label" "label" $ dynText msgUploaderList
                  elAttr "ul" ("id" =: "DirectorySettings_uploaderList") $ do
                    deleteUploaderEvtEvt <- dyn $ ffor uploadersDyn $ \uploaders -> do
                      deleteUploaderEvts <- for uploaders $ \uploader ->
                        el "li" $ do
                          el "p" $ text (convert uploader)
                          (e, _) <- buttonClassAttr "button" ("type" =: "button") $
                            elClass "span" "icon" $
                              elClass "i" "fas fa-times" blank
                          pure $ fmap (const uploader) e
                      pure $ leftmost deleteUploaderEvts
                    deleteUploaderEvt <- switchHold never deleteUploaderEvtEvt
                    addNewUploaderTi <- textInput $ TextInputConfig "text" "" never
                      (constDyn $ "class" =: "input" <> "style" =: "width: auto;")
                    (addNewUploaderEvt, _) <- buttonClassAttr "button" ("type" =: "button") $ text "添加"
                    pure mempty { deleteUploaderEvt = Last (Just deleteUploaderEvt)
                                , addNewUploaderTi = Last (Just addNewUploaderTi)
                                , addNewUploaderEvt = Last (Just addNewUploaderEvt)
                                }
              ]
          , elAttr "div" ("id" =: "DirectorySettings_btnSaveContainer") $ do
              (clickSave, _) <- buttonClassAttr "button is-primary" ("type" =: "button") $
                dynText msgSave
              pure $ mempty { clickSave = Last (Just clickSave) }
          ]
        , popupMessage DialogStyleSuccess showSaveSucceeded msgSaveSucceeded
          >> pure mempty
        , popupMessage DialogStyleWarning showSaveFailed msgSaveFailed
          >> pure mempty
        ]

      -- Messages
      msgTitle <- renderMsg' MsgTitle
      msgName <- renderMsg' MsgName
      msgDeadline <- renderMsg' MsgDeadline
      msgUploadRules <- renderMsg' MsgUploadRules
      msgFileSizeLimit <- renderMsg' MsgFileSizeLimit
      msgFileCountLimit <- renderMsg' MsgFileCountLimit
      msgFileNameFormat <- renderMsg' MsgFileNameFormat
      msgUploaderList <- renderMsg' MsgUploaderList
      msgSave <- renderMsg' MsgSave
      msgSaveSucceeded <- renderMsg' MsgSaveSucceeded
      msgSaveFailed <- renderMsg' MsgSaveFailed
      -- Handler
      HandlerResult{..} <- directorySettingsHandler handlerParams { openEvt = Last (Just openEvt) }
      blank
    where
      renderMsg' = renderMsg (Proxy :: Proxy env) DirectorySettings

data HandlerParams t = HandlerParams
  { openEvt           :: Last (Event t Directory)
  , nameTi            :: Last (TextInput t)
  , ddlDateTi         :: Last (TextInput t)
  , ddlTimeTi         :: Last (TextInput t)
  , fileSizeLimitCb   :: Last (Checkbox t)
  , fileSizeLimitTi   :: Last (TextInput t)
  , fileCountLimitCb  :: Last (Checkbox t)
  , fileCountLimitTi  :: Last (TextInput t)
  , fileNameFormatCb  :: Last (Checkbox t)
  , fileNameFormatTi  :: Last (TextInput t)
  , clickSave         :: Last (Event t ())
  , ddlCb             :: Last (Checkbox t)
  , deleteUploaderEvt :: Last (Event t UserName)
  , addNewUploaderTi  :: Last (TextInput t)
  , addNewUploaderEvt :: Last (Event t ())
  } deriving Generic

data HandlerResult t = HandlerResult
  { setNameEvt               :: Event t Text
  , cbDeadlineSetValue       :: Event t Bool
  , setDateEvt               :: Event t Text
  , setTimeEvt               :: Event t Text
  , cbFileSizeLimitSetValue  :: Event t Bool
  , setFileSizeLimit         :: Event t Text
  , cbFileCountLimitSetValue :: Event t Bool
  , setFileCountLimit        :: Event t Text
  , cbFileNameFormatSetValue :: Event t Bool
  , setFileNameFormat        :: Event t Text
  , uploadersDyn             :: Dynamic t [UserName]
  , showSaveSucceeded        :: Event t ()
  , showSaveFailed           :: Event t ()
  }

directorySettingsHandler ::
  forall t m.
  ( MonadWidget t m
  , MonadTimeZone t m
  , Service.MonadDirectoryUploaders t m
  , Service.MonadDirectory t m
  ) => HandlerParams t
    -> m (HandlerResult t)
directorySettingsHandler HandlerParams{..} = mdo
    dirNameDyn <- holdDyn (Left "") (Right . (^. directory_name) <$> openEvt')
    ownerNameDyn' <- holdDyn (UserName "") (fmap (^. directory_ownerName) openEvt')
    ownerNameDyn <- holdDyn (Left "") (Right . (^. directory_ownerName) <$> openEvt')
    getUploadersReqResult <- Service.getDirUploaders ownerNameDyn dirNameDyn (void openEvt')
    let addNewUploaderEvtWithName =
          UserName <$> tagPromptlyDyn (value addNewUploaderTi') addNewUploaderEvt'
        initialUploadersEvt = fmap (fromMaybe mempty . reqResultToMaybe) getUploadersReqResult
        updateUploadersEvt =
          leftmost [ fmap (\x xs -> filter (/= x) xs) deleteUploaderEvt'
                   , fmap const initialUploadersEvt
                   , fmap (:) addNewUploaderEvtWithName
                   ]

    -- save
    let newDirNameDyn = fmap DirectoryName (value nameTi')
        ddlUtcTime = formDateTimeToUtcTime <$> value ddlDateTi'
                                           <*> value ddlTimeTi'
        ddlUtcTime' = nothingUnless <$> value ddlCb' <*> ddlUtcTime
        fileSizeLimit :: Dynamic t (Maybe Word32) = nothingUnless
          <$> value fileSizeLimitCb'
          <*> fmap (readMaybe . T.unpack) (value fileSizeLimitTi')
        fileSizeLimitRule = (fmap . fmap) RuleMaxFileSize fileSizeLimit
        fileCountLimit = nothingUnless
          <$> value fileCountLimitCb'
          <*> fmap (readMaybe . T.unpack) (value fileCountLimitTi')
        fileCountLimitRule = (fmap . fmap) RuleMaxFiles fileCountLimit
        fileNameFormat = nothingUnless
          <$> value fileNameFormatCb'
          <*> fmap Just (value fileNameFormatTi')
        fileNameFormatRule = (fmap . fmap) RuleFileNameFormat fileNameFormat
        uploadRules = catMaybes <$> sequenceA [ fileSizeLimitRule
                                              , fileCountLimitRule
                                              , fileNameFormatRule]
        newDirectoryDyn = Directory
          <$> newDirNameDyn
          <*> ownerNameDyn'
          <*> ddlUtcTime'
          <*> uploadRules
    putDirResultEvt <-
      Service.putDir ownerNameDyn dirNameDyn (fmap Right newDirectoryDyn) clickSave'
    _ <- Service.putDirUploaders ownerNameDyn dirNameDyn (fmap Right uploadersDyn) clickSave'

    -- output
    let setNameEvt = fmap (convert . (^. directory_name)) openEvt'
        cbDeadlineSetValue =
          fmap (\dir -> isJust $ dir ^. directory_expirationTime) openEvt'
        setDateEvt = fmap (timeToFormDate . (^. directory_expirationTime)) openEvt'
        setTimeEvt = fmap (timeToFormTime . (^. directory_expirationTime)) openEvt'
        -- TODO change above utc time to zoned time
        cbFileSizeLimitSetValue = fmap (isJust . getRuleSizeLimit) openEvt'
        setFileSizeLimit =
          fmap (displayMaybe . fmap (`div` 1024) . getRuleSizeLimit) openEvt'
        cbFileCountLimitSetValue = fmap (isJust . getRuleFileCountLimit) openEvt'
        setFileCountLimit = fmap (displayMaybe . getRuleFileCountLimit) openEvt'
        cbFileNameFormatSetValue = fmap (isJust . getRuleFileNameFormat) openEvt'
        setFileNameFormat = fmap (displayMaybe . getRuleFileNameFormat) openEvt'
    uploadersDyn <- foldDyn ($) [] updateUploadersEvt
    let showSaveSucceeded = void $ ffilter isResponseSuccess putDirResultEvt
        showSaveFailed = void $ ffilter (not . isResponseSuccess) putDirResultEvt

    pure HandlerResult{..}
  where
    openEvt' = fromJust' openEvt
    nameTi' = fromJust' nameTi
    ddlDateTi' = fromJust' ddlDateTi
    ddlTimeTi' = fromJust' ddlTimeTi
    ddlCb' = fromJust' ddlCb
    fileSizeLimitCb' = fromJust' fileSizeLimitCb
    fileSizeLimitTi' = fromJust' fileSizeLimitTi
    fileCountLimitCb' = fromJust' fileCountLimitCb
    fileCountLimitTi' = fromJust' fileCountLimitTi
    fileNameFormatCb' = fromJust' fileNameFormatCb
    fileNameFormatTi' = fromJust' fileNameFormatTi
    clickSave' = fromJust' clickSave
    deleteUploaderEvt' = fromJust' deleteUploaderEvt
    addNewUploaderEvt' = fromJust' addNewUploaderEvt
    addNewUploaderTi' = fromJust' addNewUploaderTi

instance Reflex t => Semigroup (HandlerParams t) where
  (<>) = gsappenddefault

instance Reflex t => Monoid (HandlerParams t) where
  mempty = gmemptydefault
  mappend = gmappenddefault

fromJust' :: Last a -> a
fromJust' (Last (Just x)) = x
fromJust' _               = error "missing field in HandlerParams"

timeToFormDate :: FormatTime t => Maybe t -> Text
timeToFormDate Nothing  = ""
timeToFormDate (Just t) =
    T.pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) t

timeToFormTime :: FormatTime t => Maybe t -> Text
timeToFormTime Nothing = ""
timeToFormTime (Just t) =
    T.pack $ formatTime defaultTimeLocale "%H:%M" t

getRuleSizeLimit :: Directory -> Maybe Word32
getRuleSizeLimit dir =
    takeJust $ fmap (preview _RuleMaxFileSize) (dir ^. directory_uploadRules)

getRuleFileCountLimit :: Directory -> Maybe Int
getRuleFileCountLimit dir =
    takeJust $ fmap (preview _RuleMaxFiles) (dir ^. directory_uploadRules)

getRuleFileNameFormat :: Directory -> Maybe Text
getRuleFileNameFormat dir =
    takeJust $ fmap (preview _RuleFileNameFormat) (dir ^. directory_uploadRules)

takeJust :: [Maybe a] -> Maybe a
takeJust = safeHead . catMaybes

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

displayMaybe :: Show a => Maybe a -> Text
displayMaybe Nothing  = ""
displayMaybe (Just x) = T.pack . show $ x

formDateToDay :: Text -> Maybe Day
formDateToDay str = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack str)

formTimeToTimeOfDay :: Text -> Maybe TimeOfDay
formTimeToTimeOfDay str = parseTimeM True defaultTimeLocale "%H:%M" (T.unpack str)

formDateTimeToUtcTime :: Text -- ^form date
                      -> Text -- ^form time
                      -> Maybe UTCTime
formDateTimeToUtcTime formDate formTime =
    UTCTime <$> formDateToDay formDate
            <*> fmap timeOfDayToTime (formTimeToTimeOfDay formTime)

nothingUnless :: Bool -> Maybe a -> Maybe a
nothingUnless b x = if b then x else Nothing
