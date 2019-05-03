{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module FileCollector.Common.Api
  ( Api
  ) where

import qualified FileCollector.Common.Api.File as File
import qualified FileCollector.Common.Api.User as User
import           Servant.API

type Api ossProvider = "api" :>
  ( User.Api
  :<|> File.Api ossProvider
  )
