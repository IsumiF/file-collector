{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Proxy                                        (Proxy (..))
import qualified Data.Text.IO                                      as TIO
import           Servant.Docs                                      (docs)
import qualified Servant.Docs                                      as Docs
import qualified Servant.Docs.Pandoc                               as Docs (pandoc)
import qualified Text.Pandoc.Class                                 as Pandoc (runIO)
import qualified Text.Pandoc.Error                                 as Pandoc (handleError)
import           Text.Pandoc.Options                               (def)
import qualified Text.Pandoc.Writers                               as Pandoc (writeHtml5String)

import           FileCollector.Common.Api                          (Api)
import           FileCollector.Common.Types.OssProvider
import           FileCollector.Common.Types.OssProviderImpl.Aliyun (Aliyun)

main :: IO ()
main = do
    let apiDocs' = apiDocs (Proxy :: Proxy Aliyun)
    result <- Pandoc.runIO $ Pandoc.writeHtml5String def (Docs.pandoc apiDocs')
    docHtml <- Pandoc.handleError result
    TIO.putStrLn docHtml

apiDocs :: forall ossProvider. IsOssProvider ossProvider
        => Proxy ossProvider
        -> Docs.API
apiDocs _ = docs (Proxy :: Proxy (Api ossProvider))

instance Docs.ToSample () where
  toSamples _ = Docs.noSamples
