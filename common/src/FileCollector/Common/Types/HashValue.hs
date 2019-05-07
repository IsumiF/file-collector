{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module FileCollector.Common.Types.HashValue
  ( HashValue(..)
  , HashType(..)
    -- *Lens and Prisms
  , hashValue_type
  , hashValue_code
  , _HashTypeMD5
  , _HashTypeSHA3
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Base64                    as ByteStringBase64 (decode,
                                                                                encode)
import qualified Data.Text.Encoding                        as Text (decodeUtf8,
                                                                    encodeUtf8)
import           GHC.Generics                              (Generic)
import qualified Servant.Docs                              as Docs

import           Data.Aeson.Types                          (Parser)
import           FileCollector.Common.Base.Convertible
import           FileCollector.Common.Types.JsonByteString
import           FileCollector.Common.Base.Aeson          (lensDefaultOptions)

data HashValue = HashValue
  { _hashValue_type :: HashType
  , _hashValue_code :: ByteString
  } deriving (Eq, Show, Generic)

data HashType =
    HashTypeMD5
  | HashTypeSHA3
    deriving (Eq, Show, Generic)

data HashValue' = HashValue'
  { _hashValue'_type :: HashType
  , _hashValue'_code :: JsonByteString
  } deriving (Eq, Show, Generic)

instance FromJSON HashValue' where
  parseJSON = genericParseJSON lensDefaultOptions

instance ToJSON HashValue' where
  toJSON = genericToJSON lensDefaultOptions
  toEncoding = genericToEncoding lensDefaultOptions

instance Convertible HashValue' HashValue where
  convert (HashValue' t c) = HashValue t (convert c)

instance Convertible HashValue HashValue' where
  convert (HashValue t c) = HashValue' t (convert c)

instance FromJSON HashValue where
  parseJSON v = convert <$> (parseJSON v :: Parser HashValue')

instance ToJSON HashValue where
  toJSON x = toJSON (convert x :: HashValue')
  toEncoding x = toEncoding (convert x :: HashValue')

instance Docs.ToSample HashValue where
  toSamples _ = Docs.singleSample $
    HashValue
      HashTypeMD5
      "38b8c2c1093dd0fec383a9d9ac940515"

instance FromJSON HashType

instance ToJSON HashType where
  toEncoding = genericToEncoding defaultOptions

makeLenses ''HashValue
makePrisms ''HashType
