module FileCollector.Backend.Logger
  ( Logger
  , withLogStdout
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Logger hiding (filterLogger)
import System.Log.FastLogger

-- | Logger type as used by 'MonadLogger'
type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

{-| Get a 'Logger' that logs to standard output.
-}
withLogStdout :: MonadUnliftIO m
              => LogLevel -- ^minimum log level
              -> (Logger -> m a) -- ^monadic computation that uses the logger
              -> m a
withLogStdout minLogLevel action =
    withRunInIO $ \runInIO ->
      withFastLogger (LogStdout 4096) $ \fastLogger ->
        runInIO $ action' (toLogger fastLogger)
  where
    action' logger = action (filterLogger minLogLevel logger)

toLogger :: FastLogger -> Logger
toLogger fastLogger loc source level msg =
  fastLogger $ defaultLogStr loc source level msg

filterLogger :: LogLevel -> Logger -> Logger
filterLogger minLevel logger loc source level msg =
    if level < minLevel
    then pure ()
    else logger loc source level msg
