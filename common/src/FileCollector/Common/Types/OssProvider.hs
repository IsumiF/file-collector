{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module FileCollector.Common.Types.OssProvider
  ( OssProvider(..)
  , OssProviderJson
  ) where

import           Control.Lens
import           Data.Aeson
import           GHC.Generics                     (Generic)
import qualified Servant.Docs                     as Docs

import           FileCollector.Common.Base.Aeson (lensDefaultOptions)

class OssProvider provider where
  data OssClientCredential provider

type OssProviderJson provider =
  ( OssProvider provider
  , ToJSON (OssClientCredential provider)
  , FromJSON (OssClientCredential provider)
  )

instance Docs.ToSample (OssClientCredential provider) where
  toSamples _ = Docs.noSamples
