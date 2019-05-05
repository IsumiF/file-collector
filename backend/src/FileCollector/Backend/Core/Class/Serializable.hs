module FileCollector.Backend.Core.Class.Serializable
  ( ToByteString(..)
  , FromByteString(..)
  ) where

import Data.ByteString (ByteString)

class ToByteString a where
  toByteString :: a -> ByteString

class FromByteString a where
  fromByteString :: ByteString -> a

