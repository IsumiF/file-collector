{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Frontend.Core.Login
  ( combineUserEvent
  , login
  ) where

import           Control.Monad.Fix (MonadFix)
import           Data.Functor (void)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex (ReqResult (..))

import           FileCollector.Common.Types
import qualified FileCollector.Frontend.Class.Service.MonadGetUser as Service
import           FileCollector.Frontend.Types.LoggedUser

combineUserEvent :: (Reflex t, MonadHold t m)
                 => Event t ()
                 -> Event t LoggedUser
                 -> m (Dynamic t (Maybe LoggedUser))
combineUserEvent logoutEvt loginEvt = holdDyn Nothing stateEvt
  where
    logoutEvt' = fmap (const Nothing) logoutEvt
    loginEvt' = fmap Just loginEvt
    stateEvt = leftmost [loginEvt', logoutEvt']

login ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , Service.MonadGetUser t m
  ) => Event t (UserName, Password)
    -> m (Event t LoggedUser)
login credEvt = do
    let authDataEvt = fmap credToAuthData credEvt
    authDataDyn <- foldDyn (\x _ -> Just x) Nothing authDataEvt
    let userNameEvt = fmap fst credEvt
    userNameDyn <- foldDyn (\x _ -> Right x) (Left ("" :: Text)) userNameEvt
    let triggerEvt = void credEvt
    authDataTriggeredDyn <- holdDyn Nothing (tagPromptlyDyn authDataDyn triggerEvt)
    resultEvt <- Service.getUser authDataTriggeredDyn userNameDyn triggerEvt
    pure $ fmapMaybe id $
      attachPromptlyDynWith (flip resultToLoggedUser) authDataDyn resultEvt

credToAuthData :: (UserName, Password) -> BasicAuthData
credToAuthData (UserName name, Password pwd) =
    BasicAuthData (T.encodeUtf8 name) (T.encodeUtf8 pwd)

resultToLoggedUser :: ReqResult () User
                   -> Maybe BasicAuthData
                   -> Maybe LoggedUser
resultToLoggedUser reqResult authDataMaybe =
    case authDataMaybe of
      Nothing -> Nothing
      Just authData ->
        case reqResult of
          ResponseSuccess _ user _ -> Just $ LoggedUser user authData
          ResponseFailure{}        -> Nothing
          RequestFailure _ _       -> Nothing
