{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.Api
  ( 
  -- * The API
    Api
  -- * Data types
  , UserUploader(..)
  , UserCollector(..)
  , UserAdmin(..)
  , Role (..)
  , User (..)
  ) where

import qualified Isumi.FileCollector.Api.Directory as Directory
import           Isumi.FileCollector.Api.Internal.Prelude
import qualified Isumi.FileCollector.Api.User as User

type Api = BasicAuth "uploader" UserUploader :> "api" :>
    ( User.Api
    )

newtype UserUploader = UserUploader User
newtype UserCollector = UserCollector User
newtype UserAdmin = UserAdmin User
