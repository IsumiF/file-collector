{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Backend.Core.Password
  ( encryptPassword
  , checkPassword
  ) where

import           Crypto.Hash (Digest, SHA3_512, hash)
import           Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.ByteArray as ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

encryptPassword :: MonadRandom m
                => ByteString
                -> m ByteString
encryptPassword pwd = do
    salt :: ByteString <- getRandomBytes saltSize
    let hashed = hash (salt <> pwd) :: Digest SHA3_512
    pure (salt <> ByteArray.convert hashed)

checkPassword :: ByteString -- ^password in plain text
              -> ByteString -- ^hashed password
              -> Bool
checkPassword pwd cipher =
    let (salt, hashed) = ByteString.splitAt saltSize cipher
     in hashed == ByteArray.convert (hash (salt <> pwd) :: Digest SHA3_512)

saltSize :: Integral a => a
saltSize = 64
