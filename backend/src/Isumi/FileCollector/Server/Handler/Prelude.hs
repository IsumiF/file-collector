{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Isumi.FileCollector.Server.Handler.Prelude
  ( module Servant
  , AppHandler(..)
  ) where

import Control.Monad.Logger
    ( LoggingT
    )
import Control.Monad.Reader
    ( ReaderT
    )
import Data.Pool
    ( Pool
    )
import Database.Persist.Sql
    ( SqlBackend
    )
import Servant

newtype AppHandler a = AppHandler
    (LoggingT (ReaderT (Pool SqlBackend) Handler) a)
    deriving (Functor, Applicative, Monad)

