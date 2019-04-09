{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.Api.Directory
  ( Api
  , CreateDirectory(..)
  ) where

import Data.Aeson (FromJSON)
import Isumi.FileCollector.Api.Internal.Prelude

type Api = "directory" :>
    ( ReqBody '[JSON] CreateDirectory :> Post '[JSON] ()
    :<|> Get '[JSON] [Text] -- get directory names
    :<|> Capture "dir" Text :>
         ( Get '[JSON] Directory
         :<|> "files" :> Get '[JSON] [Text] -- get files inside directory
         :<|> ReqBody '[JSON] Directory :> Post '[JSON] Directory
         -- :<|> ReqBody '[JSON] File "files" :> Post '[JSON] File
         )
    )

data CreateDirectory = CreateDirectory
    deriving (Show, Eq, Generic)

instance FromJSON CreateDirectory where

-- 1. create new directory
-- 2. get names of directory i have
-- 3. get all files inside a directory
