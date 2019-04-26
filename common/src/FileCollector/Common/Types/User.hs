{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Common.Types.User
  ( User(User)
  , user_name
  , user_role
  , Role(..)
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data User = User
  { _user_name :: Text
  , _user_role :: Role
  } deriving (Show, Generic)

data Role = RoleUploader | RoleCollector | RoleAdmin
  deriving (Show, Eq, Generic)

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON User

instance ToJSON Role where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Role

makeLenses ''User

roleToInt :: Role -> Int
roleToInt RoleUploader  = 1
roleToInt RoleCollector = 2
roleToInt RoleAdmin     = 3

instance Ord Role where
  compare r1 r2 = compare (roleToInt r1) (roleToInt r2)
