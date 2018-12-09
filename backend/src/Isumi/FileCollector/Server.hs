{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Isumi.FileCollector.Server
  ( main
  ) where

import Control.Monad.Logger
    ( runStdoutLoggingT
    )
import Data.Proxy
    ( Proxy (Proxy)
    )
import Database.Persist.Sqlite
import Isumi.FileCollector.Api
    ( Api
    )
import Isumi.FileCollector.Server.Auth
    ( AuthContextEntries
    , authContext
    )
import Isumi.FileCollector.Server.Handler
import Isumi.FileCollector.Server.SqlConnPool
import Network.Wai.Handler.Warp
    ( run
    )
import Servant

type ApiWithStatic = Api
                :<|> "static" :> Raw

server :: ServerT Api AppHandler
server _ = pure 42 :<|> (const (pure 13))

serverWithStatic :: FilePath -> ServerT ApiWithStatic AppHandler
serverWithStatic staticFiles =
         server
    :<|> serveDirectoryWebApp staticFiles

app :: Application
app =
    serveWithContext
      (Proxy :: Proxy ApiWithStatic)
      authContext $
      hoistServerWithContext
        (Proxy :: Proxy ApiWithStatic)
        (Proxy :: Proxy AuthContextEntries)
        unAppHandler
        (serverWithStatic "static")

main :: IO ()
main = do
    runStdoutLoggingT $ createSqlitePool "mydb.db" 20 >>= initSqlConnPool
    run 8080 app

