{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.Api
  ( Api
  ) where

import           Isumi.FileCollector.Api.Prelude
import qualified Isumi.FileCollector.Api.User as User

type Api = BasicAuth "uploader" UserUploader :> "api" :>
    ( User.Api
    )
