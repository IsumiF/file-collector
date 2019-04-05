{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module FileCollector.Common.Api
  ( Api
  ) where

import qualified FileCollector.Common.Api.Dir as Dir
import           Servant.API

type Api = "api" :> Dir.Api
