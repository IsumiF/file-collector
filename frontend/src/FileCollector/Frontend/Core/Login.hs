module FileCollector.Frontend.Core.Login
  ( combineUserEvent
  , login
  ) where

import Reflex.Dom

import FileCollector.Common.Types
import FileCollector.Frontend.Types.LoggedUser

combineUserEvent :: Reflex t
                 => Event t ()
                 -> Event t LoggedUser
                 -> Dynamic t LoggedUser
combineUserEvent = undefined

login :: MonadWidget t m
      => Event t (UserName, Password)
      -> m (Event t LoggedUser)
login evtCred = do
    performEvent $ ffor evtCred $ \(username, password) -> do
      undefined
    pure undefined

