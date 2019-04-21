{-# LANGUAGE CPP #-}

module Main
  ( main
  ) where

#ifdef ghcjs_HOST_OS
import           FileCollector.Frontend.Main.GHCJS   (main)
#else
import           FileCollector.Frontend.Main.GHCWarp (main)
#endif
