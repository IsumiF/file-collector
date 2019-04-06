{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

import           Distribution.Simple                   (UserHooks (..),
                                                        defaultMain,
                                                        defaultMainWithHooks,
                                                        simpleUserHooks)
import           Distribution.Simple.Setup             (BuildFlags,
                                                        buildVerbosity,
                                                        fromFlagOrDefault)
import           Distribution.Simple.Utils             (copyDirectoryRecursive)
import           Distribution.Types.LocalBuildInfo     (LocalBuildInfo)
import qualified Distribution.Types.LocalBuildInfo     as LocalBuildInfo
import           Distribution.Types.PackageDescription (PackageDescription)
import qualified Distribution.Verbosity                as Verbosity
import           System.FilePath                       ((</>))

executableName :: String
executableName = "app"

main :: IO ()
main = defaultMainWithHooks customUserHooks

customUserHooks :: UserHooks
customUserHooks = simpleUserHooks
    { buildHook = customBuildHook
    }

customBuildHook :: PackageDescription
                -> LocalBuildInfo
                -> UserHooks
                -> BuildFlags
                -> IO ()
customBuildHook pkgDescr localBi hooks flags = do
    buildHook simpleUserHooks pkgDescr localBi hooks flags
    let destDir = staticFilesDest (LocalBuildInfo.buildDir localBi) executableName
    let verbosity = fromFlagOrDefault Verbosity.normal (buildVerbosity flags)
    copyDirectoryRecursive verbosity "static" destDir

staticFilesDest :: FilePath -- ^build dir
                -> String -- ^executable name
                -> FilePath
staticFilesDest buildDir exeName =
#ifdef ghcjs_HOST_OS
    buildDir </> exeName </> (exeName ++ ".jsexe") </> "static"
#else
    buildDir </> exeName </> "static"
#endif
