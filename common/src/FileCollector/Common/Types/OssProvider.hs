{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module FileCollector.Common.Types.OssProvider
  ( OssProvider(..)
  , OssProviderJson
  ) where

import           Data.Aeson
import qualified Servant.Docs                     as Docs

class OssProvider provider where
  data OssClientCredential provider

type OssProviderJson provider =
  ( OssProvider provider
  , ToJSON (OssClientCredential provider)
  , FromJSON (OssClientCredential provider)
  )

instance Docs.ToSample (OssClientCredential provider) where
  toSamples _ = Docs.noSamples
