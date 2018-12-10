{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.Api.User
  ( Api
  ) where

import Isumi.FileCollector.Api.Prelude
import Isumi.FileCollector.Server.Persist.Entity (Role)

type Api = "user" :>
    ( GetRole
    )

type GetRole = "role" :> Get '[JSON] (Maybe Role)
