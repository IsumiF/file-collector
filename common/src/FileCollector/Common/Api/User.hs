{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module FileCollector.Common.Api.User
  ( Api
  ) where

import           Data.Text                       (Text)
import           Servant.API

import           FileCollector.Common.Types.User (User)

type Api = "user" :> QueryParam "name" Text :> Get '[JSON] User
