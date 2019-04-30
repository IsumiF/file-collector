module FileCollector.Common.Utils.Aeson
  ( removeFieldNs
  , lensDefaultOptions
  ) where

import           Data.Aeson

-- | Remove the namespace part from a record field.
--
-- Example field names: @_user_name@ @_directory_uploaderName@.
-- The first underscore is mandantory.
removeFieldNs :: String -> String
removeFieldNs = tail . dropWhile (/= '_') . tail

-- | Options that remove field namespace. Suitable to use with lens records
lensDefaultOptions :: Options
lensDefaultOptions = defaultOptions { fieldLabelModifier = removeFieldNs }
