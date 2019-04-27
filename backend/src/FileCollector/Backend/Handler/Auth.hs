module FileCollector.Backend.Handler.Auth
  ( makeAuthCheck
  ) where

import Servant.Server

import FileCollector.Backend.App (AppEnv, runApp)
import FileCollector.Backend.Core.BasicAuth (authCheck)
import FileCollector.Common.Api.Auth (UserAuthWrapper)

makeAuthCheck :: UserAuthWrapper u
              => AppEnv
              -> BasicAuthCheck u
makeAuthCheck appEnv = BasicAuthCheck $
    flip runApp appEnv . authCheck
