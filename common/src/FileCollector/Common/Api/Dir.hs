{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module FileCollector.Common.Api.Dir
  ( Api
  ) where

import           Servant.API

import           FileCollector.Common.Api.Auth          (AuthUploader)
import           FileCollector.Common.Types.Directory
import           FileCollector.Common.Types.OssProvider

-- | Get all directories visible to current user
type Api = "dir" :> AuthUploader :> Get '[JSON] [Directory]
