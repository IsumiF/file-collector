{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Backend.Database.Types.Role
  ( Role(..)
  , toCommonRole
  , fromCommonRole
  ) where

import Data.Maybe (maybe)
import Database.Persist.TH (derivePersistField)
import Text.ParserCombinators.ReadPrec (look, pfail)
import Text.Read

import qualified FileCollector.Common.Types.User as Common (Role (..))

newtype Role = Role Common.Role

toCommonRole :: Role -> Common.Role
toCommonRole (Role x) = x

fromCommonRole :: Common.Role -> Role
fromCommonRole = Role

instance Show Role where
  show (Role x) =
    case x of
      Common.RoleUploader  -> "uploader"
      Common.RoleCollector -> "collector"
      Common.RoleAdmin     -> "admin"

instance Read Role where
  readPrec = do
    str <- look
    maybe pfail pure (strToRole str)

strToRole :: String -> Maybe Role
strToRole "uploader"  = Just $ Role Common.RoleUploader
strToRole "collector" = Just $ Role Common.RoleCollector
strToRole "admin"     = Just $ Role Common.RoleAdmin
strToRole _           = Nothing

derivePersistField "Role"
