module FileCollector.Backend.Logger
  ( Logger
  , newLoggerStdout
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger hiding (filterLogger)
import System.Log.FastLogger

-- | Logger type as used by 'MonadLogger'
type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

newLoggerStdout :: MonadIO m
                => LogLevel
                -> m (Logger, IO ())
newLoggerStdout minLogLevel = do
    (fastLogger, cleanup) <- liftIO $ newFastLogger (LogStdout 4096)
    let logger' = filterLogger minLogLevel (toLogger fastLogger)
    pure (logger', cleanup)

toLogger :: FastLogger -> Logger
toLogger fastLogger loc source level msg =
  fastLogger $ defaultLogStr loc source level msg

filterLogger :: LogLevel -> Logger -> Logger
filterLogger minLevel logger loc source level msg =
    if level < minLevel
    then pure ()
    else logger loc source level msg
