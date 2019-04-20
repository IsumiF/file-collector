{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Common.Types.User
  ( User
  , user_name
  , user_role
  , Role(..)
  ) where

import           Control.Lens
import           Data.Text    (Text)

data User = User
  { _user_name :: Text
  , _user_role :: Role
  , _user_preferredLanguage :: Text
  } deriving Show

data Role = RoleUploader | RoleCollector | RoleAdmin
  deriving (Show, Eq)

makeLenses ''User

roleToInt :: Role -> Int
roleToInt RoleUploader  = 1
roleToInt RoleCollector = 2
roleToInt RoleAdmin     = 3

instance Ord Role where
  compare r1 r2 = compare (roleToInt r1) (roleToInt r2)
