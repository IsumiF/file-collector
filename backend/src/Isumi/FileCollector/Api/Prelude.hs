module Isumi.FileCollector.Api.Prelude
  ( module Prelude
  , module Servant.API
  , UserCollector
  , UserUploader
  , UserAdmin
  ) where

import Isumi.FileCollector.Server.Auth
    ( UserAdmin
    , UserCollector
    , UserUploader
    )
import Servant.API
