{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module FileCollector.Common.Types.OssProvider
  ( OssProvider(..)
  , IsOssProvider
  ) where

import           Data.Aeson

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
