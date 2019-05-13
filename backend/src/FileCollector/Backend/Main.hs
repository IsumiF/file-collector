{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Backend.Main
  ( main
  ) where

import           Control.Lens hiding (Context)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
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
import qualified FileCollector.Backend.Oss.Aliyun as Aliyun
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
        -- 阿里云演示
        -- initStatus <- Aliyun.initialize
        -- if not initStatus
        -- then putStrLn "阿里云初始化失败"
        -- else do
        --   let aliyunConfig = config ^. (config_oss . configOss_aliyun)
        --   let accessKey = Aliyun.AccessKey
        --         (aliyunConfig ^. configOssAliyun_accessKeyId)
        --         (aliyunConfig ^. configOssAliyun_accessKeySecret)
        --   let objectId = Aliyun.ObjectId
        --         (aliyunConfig ^. configOssAliyun_endPoint)
        --         (aliyunConfig ^. configOssAliyun_bucketName)
        --         "hello.txt"
        --   uploadUrl <- Aliyun.getUploadUrl accessKey objectId
        --   T.putStrLn uploadUrl

        sqlPool :: Pool SqlBackend <- runStdoutLoggingT $
          createSqlitePool (config ^. config_dbConnStr) 10
        withLogStdout (coerce (config ^. config_logLevel)) $ \logger -> do
          let appEnv = makeAppEnv sqlPool logger (config ^. config_oss)
          flip runApp appEnv $ do
            Database.initialize
            aliyunStatus <- liftIO $ Aliyun.initialize
            if not aliyunStatus
            then $(logError) "Aliyun initialization failed" >> exitFailure'
            else pure ()
          Warp.run (config ^. config_port) (makeApplication appEnv)

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

makeApplication :: AppEnv
                -> Application
makeApplication env =
    serveWithContext
      apiProxy
      context
      (hoistServerWithContext apiProxy contextProxy hoist handler)
  where
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
