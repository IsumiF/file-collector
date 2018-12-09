{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Isumi.FileCollector.Api
  ( Api
  ) where

import Isumi.FileCollector.Api.Prelude

type Api = BasicAuth "uploader" UserUploader :> "api"
    :> (ApiUpload :<|> ApiViewAll)

type ApiUpload = "upload" :> Get '[JSON] Int

type ApiViewAll = BasicAuth "collector" UserCollector
    :> "view-all" :> Get '[JSON] Int
