{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
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
import           System.FilePath                        (takeDirectory)

main :: IO ()
main = do
    appPath <- currentPath
    runWithDirectory 8081 (takeDirectory appPath) jsmMain

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
