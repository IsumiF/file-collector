{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module FileCollector.Frontend.Main.GHCWarp
  ( main
  ) where

import           FileCollector.Frontend.Main            (jsmMain)
import           FileCollector.Frontend.Utils           (currentPath)
import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets (jsaddleApp, jsaddleOr)
import qualified Network.Wai                            as Wai
import           Network.Wai.Application.Static         (defaultWebAppSettings,
                                                         staticApp)
import qualified Network.Wai.Handler.Warp               as Warp (run)
import           Network.WebSockets.Connection          (defaultConnectionOptions)
import           System.FilePath                        (takeDirectory, (</>))
import qualified Options.Applicative as Opt

data Options =
  Options
    Int -- ^port
    FilePath -- ^resource files base directory

main :: IO ()
main = do
    appPath <- currentPath
    (Options port resourceRoot) <- Opt.execParser (optionsParserInfo (takeDirectory appPath))
    putStrLn $ "Running on port " ++ show port
    putStrLn $ "Resource root: " ++ resourceRoot
    runWithDirectory port resourceRoot jsmMain

optionsParser :: FilePath -> Opt.Parser Options
optionsParser defResourceRoot = Options
    <$> Opt.option Opt.auto
        ( Opt.long "port"
       <> Opt.short 'p'
       <> Opt.metavar "PORT"
       <> Opt.value defaultPort
       <> Opt.showDefault
        )
    <*> Opt.strOption
        ( Opt.long "resource-root"
       <> Opt.short 'r'
       <> Opt.metavar "RESOURCE"
       <> Opt.value defResourceRoot
       <> Opt.showDefaultWith id
        )

defaultPort :: Int
defaultPort = 8081

optionsParserInfo :: FilePath -> Opt.ParserInfo Options
optionsParserInfo defResourceRoot =
    Opt.info ((optionsParser defResourceRoot) Opt.<**> Opt.helper) Opt.fullDesc

runWithDirectory :: Int -> FilePath -> JSM () -> IO ()
runWithDirectory port dir jsm = do
    jsApp <- jsaddleOr defaultConnectionOptions (jsm >> syncPoint) jsaddleApp
    let staticApp' = staticApp (defaultWebAppSettings dir)
    Warp.run port $ \request respond ->
      (case Wai.pathInfo request of
        []             -> jsApp
        ["jsaddle.js"] -> jsApp
        ("sync":_)     -> jsApp
        _              -> staticApp') request respond
