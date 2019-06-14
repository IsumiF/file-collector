{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Distribution.Simple                   (UserHooks (..),
                                                        defaultMain,
                                                        defaultMainWithHooks,
                                                        simpleUserHooks)
import           Distribution.Simple.Compiler          (OptimisationLevel (..))
import           Distribution.Simple.Setup             (BuildFlags,
                                                        buildVerbosity,
                                                        fromFlagOrDefault)
import           Distribution.Simple.Utils             (copyFiles,
                                                        rawSystemExit)
import           Distribution.Types.LocalBuildInfo     (LocalBuildInfo)
import qualified Distribution.Types.LocalBuildInfo     as LocalBuildInfo
import           Distribution.Types.PackageDescription (PackageDescription)
import           Distribution.Verbosity                (Verbosity)
import qualified Distribution.Verbosity                as Verbosity
import           System.Directory                      (getCurrentDirectory,
                                                        setCurrentDirectory)
import           System.FilePath                       ((</>))

executableName :: String
executableName = "client"

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
    let optimisationLevel = LocalBuildInfo.withOptimization localBi

    let buildConfig = BuildConfig
          "static" destDir verbosity optimisationLevel

    runReaderT buildWebStatic buildConfig

staticFilesDest :: FilePath -- ^build dir
                -> String -- ^executable name
                -> FilePath
staticFilesDest buildDir exeName =
#ifdef ghcjs_HOST_OS
    buildDir </> exeName </> (exeName ++ ".jsexe")
#else
    buildDir </> exeName </> "static"
#endif

data BuildConfig = BuildConfig
  { buildConfig_sourceDir         :: FilePath
  , buildConfig_buildDir          :: FilePath
  , buildConfig_verbosity         :: Verbosity
  , buildConfig_optimisationLevel :: OptimisationLevel
  }

buildWebStatic :: (MonadReader BuildConfig m, MonadIO m)
               => m ()
buildWebStatic = do
  config <- ask
  let sourceDir = buildConfig_sourceDir config
      buildDir = buildConfig_buildDir config

  -- copy favicon.ico
  copyFiles' buildDir [(sourceDir, "favicon.ico")]

  -- build scss files
  rawSystemExit' "sass"
    [ "--no-source-map"
    , "--update"
    , "--load-path=" <> (sourceDir </> "extern")
    , (sourceDir </> "styles") <> ":" <> (buildDir </> "styles")
    ]
  onProductionBuild $
    withCurrentDirectory' (buildDir </> "styles") $
      rawSystemExit' "cleancss"
        [ "-O2"
        , "--output"
        , "all.min.css"
        , "all.css"
        ]

  -- copy js files
  copyFiles' buildDir [(sourceDir, "js" </> "TimeZone.js")]

class HasOptimisationLevel env where
  getOptimisationLevel :: env -> OptimisationLevel

instance HasOptimisationLevel BuildConfig where
  getOptimisationLevel = buildConfig_optimisationLevel

class HasVerbosity env where
  getVerbosity :: env -> Verbosity

instance HasVerbosity BuildConfig where
  getVerbosity = buildConfig_verbosity

onProductionBuild :: (MonadReader env m, HasOptimisationLevel env, MonadIO m)
                  => m ()
                  -> m ()
onProductionBuild action = do
  optimisationLevel <- getOptimisationLevel <$> ask
  if optimisationLevel == MaximumOptimisation
  then action
  else pure ()

rawSystemExit' :: (MonadReader env m, HasVerbosity env, MonadIO m)
               => FilePath
               -> [String]
               -> m ()
rawSystemExit' command args = do
  verbosity <- getVerbosity <$> ask
  liftIO $ rawSystemExit verbosity command args

copyFiles' :: (MonadReader env m, HasVerbosity env, MonadIO m)
           => FilePath
           -> [(FilePath, FilePath)]
           -> m ()
copyFiles' destDir sourceFiles = do
  verbosity <- getVerbosity <$> ask
  liftIO $ copyFiles verbosity destDir sourceFiles

withCurrentDirectory' :: MonadIO m => FilePath -> m a -> m a
withCurrentDirectory' tempCurrentDirectory action = do
  currentDirectory <- liftIO $ getCurrentDirectory
  liftIO $ setCurrentDirectory tempCurrentDirectory
  result <- action
  liftIO $ setCurrentDirectory currentDirectory
  pure result
