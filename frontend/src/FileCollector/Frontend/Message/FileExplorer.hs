{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module FileCollector.Frontend.Message.FileExplorer
  ( FileExplorer(..)
  , FileExplorerMessage(..)
  , renderMsg
  , renderMsgDyn
  ) where

import FileCollector.Frontend.Message.Prelude

data FileExplorer = FileExplorer
mkMessage "FileExplorer" "values/messages/file_explorer" "zh-CN"
