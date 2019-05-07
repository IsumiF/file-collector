{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module FileCollector.Common.Types.User
  ( User(User)
  , UserName(..)
  , Role(..)
    -- *Lens
  , user_name
  , user_role
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant.API
import qualified Servant.Docs as Docs

import FileCollector.Common.Base.Aeson (lensDefaultOptions)

data User = User
  { _user_name :: UserName
  , _user_role :: Role
  } deriving (Show, Generic)

newtype UserName = UserName Text
  deriving (Show, Eq, Ord, Generic, FromHttpApiData, ToHttpApiData)

data Role = RoleUploader | RoleCollector | RoleAdmin
  deriving (Show, Eq, Generic)

instance ToJSON User where
  toJSON = genericToJSON lensDefaultOptions
  toEncoding = genericToEncoding lensDefaultOptions

instance FromJSON User where
  parseJSON = genericParseJSON lensDefaultOptions

instance ToJSON UserName where
  toJSON (UserName x) = toJSON x
  toEncoding (UserName x) = toEncoding x

instance FromJSON UserName where
  parseJSON v = UserName <$> parseJSON v

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

instance Docs.ToSample User where
  toSamples _ = Docs.singleSample $ User (UserName "zelinf") RoleUploader

instance Docs.ToSample UserName where
  toSamples _ = Docs.singleSample (UserName "Isumi Fly")

instance Docs.ToSample Role where
  toSamples _ = Docs.samples [RoleUploader, RoleCollector, RoleAdmin]
