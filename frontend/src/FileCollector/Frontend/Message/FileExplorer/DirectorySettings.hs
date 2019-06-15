{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module FileCollector.Frontend.Message.FileExplorer.DirectorySettings
  ( DirectorySettings(..)
  , DirectorySettingsMessage(..)
  , renderMsg
  , renderMsgDyn
  ) where

import FileCollector.Frontend.Message.Prelude

data DirectorySettings = DirectorySettings
mkMessage "DirectorySettings" "values/messages/directory_settings" "zh-CN"
