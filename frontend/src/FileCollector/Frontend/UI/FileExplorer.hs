{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.UI.FileExplorer
  ( widget
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Foldable (traverse_)
import           Data.Maybe (isJust)
import           Data.Proxy
import qualified Data.Text as T
import           Reflex.Dom
import           Servant.Reflex (ReqResult (..))

import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types
import           FileCollector.Frontend.Class.Language
import qualified FileCollector.Frontend.Class.Service.MonadDirectory as Service
import           FileCollector.Frontend.Class.User
import           FileCollector.Frontend.Message.FileExplorer

widget ::
  ( MonadWidget t m
  , Service.MonadDirectory t m
  , MonadReader env m
  , HasLanguage t env
  , HasUser t env
  ) => m ()
widget = showIfLoggedIn $ mdo
    viewingDirEvt' <- dyn $ ffor viewingDirDyn $ \case
      Nothing -> (fmap . fmap) Just directoryWidget
      Just viewingDir ->  (fmap . fmap) (const Nothing) (fileWidget viewingDir)
    viewingDirEvt <- switchHold never viewingDirEvt'
    viewingDirDyn <- holdDyn Nothing viewingDirEvt

    blank

showIfLoggedIn ::
  ( MonadWidget t m
  , MonadReader env m
  , HasUser t env
  ) => m ()
    -> m ()
showIfLoggedIn sub = do
    userDyn <- asks getUser
    dyn_ $ ffor userDyn $ \userMaybe -> if isJust userMaybe then sub else blank

-- |File list in a directory
fileWidget :: MonadWidget t m
           => Directory
           -> m (Event t ()) -- ^on click back
fileWidget dir = do
    text ("showing files of " <> convert (dir ^. directory_name))
    pure never

-- |Directory list
directoryWidget ::
  ( MonadWidget t m
  , Service.MonadDirectory t m
  , MonadReader env m
  , HasLanguage t env
  ) => m (Event t Directory) -- ^on click specific directory
directoryWidget = mdo
    elClass "nav" "panel" $ do
      elAttr "p" ("id" =: "FileExplorer_heading") $ dynText dirListHeaderDyn
      divClass "panel-block" $ text "下载"
      divClass "panel-block" $
        elAttr "table" ("id" =: "FileExplorer_table") $ mdo
          el "colgroup" $ do
            elAttr "col" ("style" =: "width: fit-content") blank
            elAttr "col" ("style" =: "width: fill") blank
            elAttr "col" ("style" =: "width: fit-content") blank
          selectAllCb <- el "thead" $
            el "tr" $ do
              el "th" $ dynText msgDirNameDyn
              el "th" $ dynText msgDirOwnerDyn
              el "th" $
                elClass "label" "checkbox" $ do
                  dynText msgSelectAllDyn
                  checkbox False selectAllConfig
          anyUnselectEvt <- el "tbody" $ do
            anyUnselectEvt' <- dyn $ ffor dirListDyn $ \dirs -> do
              unselectEvts <- traverse (showDir (selectAllCb ^. checkbox_change)) dirs
              pure $ leftmost unselectEvts
            switchHold never anyUnselectEvt'

          let selectAllConfig = CheckboxConfig (fmap (const False) anyUnselectEvt) (constDyn mempty)
          
          blank

    dirListHeaderDyn <- renderMsg' MsgDirectoryListHeader
    msgSelectAllDyn <- renderMsg' MsgSelectAll
    msgDirNameDyn <- renderMsg' MsgDirName
    msgDirOwnerDyn <- renderMsg' MsgDirOwner

    postBuildEvt <- getPostBuild
    dirListResultEvt <- Service.getDirList postBuildEvt
    let dirListEvt = ffor dirListResultEvt $ \case
          ResponseSuccess _ xs _ -> xs
          _ -> []
    dirListDyn <- holdDyn [] dirListEvt

    pure never

  where
    pEnv = Proxy :: Proxy env
    renderMsg' = renderMsg pEnv FileExplorer

showDir :: MonadWidget t m
        => Event t Bool -- ^select all
        -> Directory
        -> m (Event t ())
showDir selectAllChangeEvt dir = mdo
    cbChangeEvt <- el "tr" $ do
      el "td" $ text (convert $ dir ^. directory_name)
      el "td" $ text (convert $ dir ^. directory_ownerName)
      elAttr "td" ("id" =: "FileExplorer_dirCheckboxCell") $
        elClass "label" "checkbox" $ do
          (Checkbox _ cbChangeEvt') <- checkbox False cbConf
          pure cbChangeEvt'

    let selectAllEvt = ffilter id selectAllChangeEvt
        cbConf = CheckboxConfig selectAllEvt (constDyn mempty)
    pure $ fmapMaybe (\x -> if x then Nothing else Just ()) cbChangeEvt
