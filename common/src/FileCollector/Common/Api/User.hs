{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module FileCollector.Common.Api.User
  ( Api
  ) where

import           Data.Text                       (Text)
import           Servant.API

import           FileCollector.Common.Api.Auth   (AuthUploader)
import           FileCollector.Common.Types.User (User)

type Api = AuthUploader :> "user" :> QueryParam "name" Text :> Get '[JSON] User
