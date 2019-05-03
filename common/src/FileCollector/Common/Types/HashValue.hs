{-# LANGUAGE OverloadedStrings #-}

module FileCollector.Common.Types.HashValue
  ( HashValue(..)
  ) where

import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as ByteStringBase64 (decode, encode)
import qualified Data.Text.Encoding     as Text (decodeUtf8, encodeUtf8)
import qualified Servant.Docs           as Docs

newtype HashValue = HashValue ByteString
    deriving (Eq, Show)

instance FromJSON HashValue where
  parseJSON = withText "Expected text" $ \txt ->
    case ByteStringBase64.decode . Text.encodeUtf8 $ txt of
      Left errMsg -> fail errMsg
      Right bs    -> pure (HashValue bs)

instance ToJSON HashValue where
  toJSON (HashValue bs) =
    toJSON . Text.decodeUtf8 . ByteStringBase64.encode $ bs

instance Docs.ToSample HashValue where
  toSamples _ = Docs.singleSample $ HashValue "38b8c2c1093dd0fec383a9d9ac940515"
