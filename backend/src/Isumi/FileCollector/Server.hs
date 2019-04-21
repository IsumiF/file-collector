{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Isumi.FileCollector.Server
  ( main
  , mainWith
  , ServerConfig
  , serverConfigPort
  , serverConfigDbString
  , serverConfigDbPoolSize
  ) where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Default
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Database.Persist.Sqlite
import Isumi.FileCollector.Api (Api)
import Isumi.FileCollector.Server.Auth (AuthContextEntries, authContext)
import Isumi.FileCollector.Server.Handler
import Isumi.FileCollector.Server.SqlConnPool
import Network.Wai.Handler.Warp (Port, run)
import Servant

type ApiWithStatic = Api
                :<|> "static" :> Raw

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

mainWith :: ServerConfig -> IO ()
mainWith ServerConfig{..} = do
    runStdoutLoggingT $
      createSqlitePool serverConfigDbString serverConfigDbPoolSize
        >>= initSqlConnPool
    run serverConfigPort app

data ServerConfig = ServerConfig
    { serverConfigPort       :: Port
    , serverConfigDbString   :: Text
    , serverConfigDbPoolSize :: Int
    } deriving Show

instance Default ServerConfig where
    def = ServerConfig 8080 "file-collector.db" 30

main :: IO ()
main = mainWith def
