{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module FileCollector.Backend.Main
  ( main
  , mainAsWai
  ) where

import           Control.Lens hiding (Context)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Coerce (coerce)
import           Data.Pool (Pool)
import           Data.Proxy (Proxy (Proxy))
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Sqlite (createSqlitePool)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors
import qualified Options.Applicative as Opt
import           Servant.Server
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           FileCollector.Backend.App (App, AppEnv, makeAppEnv, runApp)
import           FileCollector.Backend.Config
import qualified FileCollector.Backend.Database as Database (initialize)
import qualified FileCollector.Backend.Database.Class.MonadConnection as Db
    (withConnection)
import           FileCollector.Backend.Handler
    (handler, makeAuthCheck, toHandler)
import           FileCollector.Backend.Logger (newLoggerStdout)
import qualified FileCollector.Backend.Oss.Impl.Aliyun as Aliyun
import           FileCollector.Common.Api (Api)
import           FileCollector.Common.Api.Auth
    (UserAdmin, UserCollector, UserUploader)
import           FileCollector.Common.Types.OssProviderImpl.Aliyun (Aliyun)

-- | Entry point of the server
main :: IO ()
main = do
    option <- Opt.execParser parserInfo
    maybeConfig <- readConfigFromFile (option_configFile option)
    case maybeConfig of
      Nothing -> do
        hPutStrLn stderr "Can't read configuration file"
        exitFailure'
      Just config -> do
          warpApp <- mainAsWai' (config ^. config_other) (pure ())
          let warpSettings =
                Warp.setPort (config ^. (config_warp . warpConfig_port))
                Warp.defaultSettings
          Warp.runSettings warpSettings warpApp

mainAsWai' :: OtherConfig -> App () -> IO Application
mainAsWai' config postAppInit = do
    (app, _) <- mainAsWai config postAppInit
    pure app

-- | Initializes the server and returns a wai 'Application'
mainAsWai :: OtherConfig
          -> App a -- ^code to run after initialized the app
          -> IO (Application, a)
mainAsWai config postAppInit = do
    (logger, _) <- newLoggerStdout (coerce (config ^. otherConfig_logLevel))
    sqlPool :: Pool SqlBackend <- flip runLoggingT logger $
      createSqlitePool (config ^. otherConfig_dbConnStr) 10
    let appEnv = makeAppEnv sqlPool logger (config ^. otherConfig_oss)
    flip runApp appEnv $ do
      Db.withConnection Database.initialize
      aliyunStatus <- liftIO Aliyun.initialize
      x <- if not aliyunStatus
           then $(logError) "Aliyun initialization failed" >> exitFailure'
           else postAppInit
      let app = servantApp appEnv
          app' = cors (const $ Just corsPolicy) app
      pure (app', x)

exitFailure' :: MonadIO m => m a
exitFailure' = liftIO exitFailure

newtype Option = Option
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

servantApp :: AppEnv
           -> Application
servantApp env =
    serveWithContext
      apiProxy
      context
      (hoistServerWithContext apiProxy contextProxy hoist (handler ossProxy))
  where
    ossProxy = Proxy :: Proxy Aliyun
    apiProxy = Proxy :: Proxy (Api Aliyun)
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

corsPolicy :: CorsResourcePolicy
corsPolicy =
  let requestHeaders = "content-type" : "Authorization" : corsRequestHeaders simpleCorsResourcePolicy
  in simpleCorsResourcePolicy { corsRequestHeaders = requestHeaders }
