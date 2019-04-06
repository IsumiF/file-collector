module FileCollector.Frontend.Utils
  ( currentPath
  ) where

import           Foreign
import           Foreign.C

foreign import ccall "current_path"
    c_currentPath :: Ptr CChar -> CSize -> IO ()

currentPath :: IO FilePath
currentPath = do
    let size = 256 :: CSize
    allocaArray (fromIntegral size) $ \c_path -> do
      c_currentPath c_path size
      peekCString c_path
