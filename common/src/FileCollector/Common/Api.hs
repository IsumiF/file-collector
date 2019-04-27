{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module FileCollector.Common.Api
  ( Api
  ) where

import qualified FileCollector.Common.Api.Dir  as Dir
import qualified FileCollector.Common.Api.User as User
import           Servant.API

type Api = "api" :> User.Api
