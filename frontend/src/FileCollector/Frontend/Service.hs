{-# LANGUAGE ScopedTypeVariables #-}

module FileCollector.Frontend.Service
  ( generateServiceAccessors
  ) where

import Control.Monad.Reader
import Data.Proxy (Proxy (..))
import Reflex.Dom
import Servant.API
import Servant.Reflex

import FileCollector.Common.Api (Api)
import FileCollector.Common.Types
import FileCollector.Frontend.Class.BaseUrl
import FileCollector.Frontend.Types.ServiceAccessors

generateServiceAccessors ::
  forall t m env.
  ( MonadWidget t m
  , HasBaseUrl env
  )
  => ReaderT env m (ServiceAccessors t m)
generateServiceAccessors = do
    baseUrl <- asks getBaseUrl
    let fileFuncs :<|> userFuncs = client (Proxy :: Proxy (Api Aliyun))
                                          (Proxy :: Proxy m)
                                          (Proxy :: Proxy ())
                                          (constDyn baseUrl)
        dirFuncs :<|> batchDownload = fileFuncs
        ( getDirList
          :<|> getDir
          :<|> putDir
          :<|> deleteDir
          :<|> dirUploaders
          :<|> getDirContent
          :<|> getFile
          :<|> putFile
          :<|> deleteFile
          :<|> commitPutFile
          ) = dirFuncs
        getUser = userFuncs
        putDirUploaders x y z a b =
          let (f :<|> _) = dirUploaders x y z
           in f a b
        getDirUploaders x y z a =
          let (_ :<|> f) = dirUploaders x y z
           in f a
    lift $ pure $ ServiceAccessors
      getDirList
      getDir
      putDir
      deleteDir
      getDirUploaders
      putDirUploaders
      getDirContent
      getFile
      putFile
      deleteFile
      commitPutFile
      batchDownload
      getUser
