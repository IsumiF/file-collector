{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Common.Base.MonadConvertible
  ( MonadConvertible(..)
  ) where

class Monad m => MonadConvertible a b m where
  convertM :: a -> m b
