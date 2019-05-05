{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileCollector.Common.Types.Convertible
  ( Convertible (..)
  ) where

import           Data.Coerce

class Convertible a b where
  convert :: a -> b

instance {-# OVERLAPPABLE #-} (Coercible a b) => Convertible a b where
  convert = coerce
