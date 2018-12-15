{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.Api.User
  ( Api
  ) where

import Isumi.FileCollector.Api.Internal.Prelude

type Api = "user" :>
    ( RoleGet
    )

type RoleGet = "role" :> Get '[JSON] (Maybe Role)
