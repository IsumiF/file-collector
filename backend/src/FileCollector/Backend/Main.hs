{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module FileCollector.Backend.Main
  ( main
  ) where

import           Control.Lens hiding (Context)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Data.Coerce (coerce)
import           Data.Pool (Pool)
import           Data.Proxy (Proxy (Proxy))
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Sqlite (createSqlitePool)
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Options.Applicative as Opt
import           Servant.Server
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           FileCollector.Backend.App (AppEnv, makeAppEnv, runApp)
import           FileCollector.Backend.Config
import qualified FileCollector.Backend.Database as Database (initialize)
import           FileCollector.Backend.Handler
    (handler, makeAuthCheck, toHandler)
import           FileCollector.Backend.Logger (withLogStdout)
import           FileCollector.Common.Api (Api)
import           FileCollector.Common.Api.Auth
    (UserAdmin, UserCollector, UserUploader)

-- | Entry point of the server
main :: IO ()
main = do
    option <- Opt.execParser parserInfo
    maybeConfig <- readConfigFromFile (option_configFile option)
    case maybeConfig of
      Nothing -> do
        hPutStrLn stderr "Can't read configuration file"
        exitFailure
      Just config -> do
        sqlPool :: Pool SqlBackend <- runStdoutLoggingT $
          createSqlitePool (config ^. config_dbConnStr) 10
        withLogStdout (coerce (config ^. config_logLevel)) $ \logger -> do
          let appEnv = makeAppEnv sqlPool logger
          runApp Database.initialize appEnv
          Warp.run (config ^. config_port) (makeApplication appEnv)

data Option = Option
  { option_configFile :: FilePath
  }

parser :: Opt.Parser Option
parser = Option
    <$> Opt.strOption
        ( Opt.long "config"
       <> Opt.short 'c'
       <> Opt.metavar "CONFIG_FILE"
       <> Opt.help "Path to configuration file"
        )

parserInfo :: Opt.ParserInfo Option
parserInfo = Opt.info (parser Opt.<**> Opt.helper) Opt.fullDesc

makeApplication :: AppEnv
                -> Application
makeApplication env =
    serveWithContext
      apiProxy
      context
      (hoistServerWithContext apiProxy contextProxy hoist handler)
  where
    apiProxy = Proxy :: Proxy Api
    context = makeContext env
    contextProxy = Proxy :: Proxy HandlerContext
    hoist = toHandler env

type HandlerContext =
       ( BasicAuthCheck UserAdmin
      ': BasicAuthCheck UserCollector
      ': BasicAuthCheck UserUploader
      ': '[] )

makeContext :: AppEnv -> Context HandlerContext
makeContext env =
       makeAuthCheck env
    :. makeAuthCheck env
    :. makeAuthCheck env
    :. EmptyContext
