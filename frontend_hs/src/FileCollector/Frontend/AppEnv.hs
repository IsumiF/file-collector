module FileCollector.Frontend.AppEnv
    ( AppEnv
    ) where

import           FileCollector.Common.Types.User

data AppEnv = AppEnv
    { user :: User
    }
