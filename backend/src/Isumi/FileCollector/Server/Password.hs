{-# LANGUAGE ScopedTypeVariables #-}

module Isumi.FileCollector.Server.Password
  ( hashPwd
  , verifyPwd
  ) where

import           Crypto.Hash (Digest, SHA3_512, hash)
import           Crypto.Random
import qualified Data.ByteArray as ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

saltSize :: Integral a => a
saltSize = 64

hashPwd :: ByteString -> IO ByteString
hashPwd pwd = do
    salt :: ByteString <- getRandomBytes saltSize
    let hashed = hash (salt <> pwd) :: Digest SHA3_512
    pure (salt <> ByteArray.convert hashed)

verifyPwd :: ByteString -- ^password in plain text
          -> ByteString -- ^previously hashed password
          -> Bool
verifyPwd pwd cipherText =
    let (salt, hashed) = ByteString.splitAt saltSize cipherText
     in hashed == ByteArray.convert (hash (salt <> pwd) :: Digest SHA3_512)

