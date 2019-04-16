{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.Utils
  ( currentPath
  , addHeaderContentTypeJson
  ) where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Foreign         (Ptr, allocaArray)
import           Foreign.C       (CChar(..), CSize(..), peekCString)
import           Reflex.Dom

foreign import ccall "current_path"
    c_currentPath :: Ptr CChar -> CSize -> IO ()

currentPath :: IO FilePath
currentPath = do
    let size = 256 :: CSize
    allocaArray (fromIntegral size) $ \c_path -> do
      c_currentPath c_path size
      peekCString c_path

addHeaderContentTypeJson :: XhrRequest a
                         -> XhrRequest a
addHeaderContentTypeJson request =
    over
      (xhrRequest_config . xhrRequestConfig_headers)
      (Map.insert "Accept" "application/json")
      request
