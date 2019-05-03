{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module FileCollector.Common.Types.OssProvider
  ( OssProvider(..)
  , IsOssProvider
  , FileCredential(..)
  ) where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics                     (Generic)
import qualified Servant.Docs                     as Docs

import           FileCollector.Common.Utils.Aeson (lensDefaultOptions)

class OssProvider provider where
  type Credential provider = a | a -> provider
  type FileLocation provider = a | a -> provider

type IsOssProvider provider =
  ( OssProvider provider
  , ToJSON (Credential provider)
  , FromJSON (Credential provider)
  , ToJSON (FileLocation provider)
  , FromJSON (FileLocation provider)
  )

data FileCredential provider = FileCredential
  { _fileCred_credential :: Credential provider
  , _fileCred_file       :: FileLocation provider
  } deriving Generic

makeLenses ''FileCredential

instance IsOssProvider provider => FromJSON (FileCredential provider) where
  parseJSON = genericParseJSON lensDefaultOptions

instance IsOssProvider provider => ToJSON (FileCredential provider) where
  toJSON = genericToJSON lensDefaultOptions
  toEncoding = genericToEncoding lensDefaultOptions

instance IsOssProvider provider => Docs.ToSample (FileCredential provider) where
  toSamples _ = Docs.noSamples
