{-# LANGUAGE DeriveGeneric #-}

module FileCollector.Common.Types.JsonByteString
  ( JsonByteString(..)
  ) where

import           Data.Aeson
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Base64                 as ByteStringBase64 (decode,
                                                                             encode)
import qualified Data.Text.Encoding                     as Text (decodeUtf8,
                                                                 encodeUtf8)
import           GHC.Generics                           (Generic)

import           FileCollector.Common.Base.Convertible

newtype JsonByteString = JsonByteString ByteString
  deriving (Eq, Show, Generic)

instance FromJSON JsonByteString where
  parseJSON = withText "Expected text" $ \txt ->
    case ByteStringBase64.decode . Text.encodeUtf8 $ txt of
      Left errMsg -> fail errMsg
      Right bs    -> pure (JsonByteString bs)

instance ToJSON JsonByteString where
  toJSON (JsonByteString bs) =
    toJSON . Text.decodeUtf8 . ByteStringBase64.encode $ bs
