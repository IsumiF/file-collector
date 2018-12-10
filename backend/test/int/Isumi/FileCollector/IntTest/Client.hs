{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.IntTest.Client
  ( userRole
  , module Servant.Client
  , module Servant.API
  , module Isumi.FileCollector.Api
  ) where

import Data.Proxy
import Isumi.FileCollector.Api
import Servant.API
import Servant.Client

userRole :: BasicAuthData -> ClientM (Maybe Role)
userRole = client (Proxy :: Proxy Api)

