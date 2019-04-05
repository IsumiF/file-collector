{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module FileCollector.Common.Api.Dir
  ( Api
  ) where

import           FileCollector.Common.Types.Directory
import           Servant.API

type Api = "dir" :> Get '[JSON] [Directory]
