{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module FileCollector.Frontend.Message
  (
  ) where

import           Text.Shakespeare.I18N (RenderMessage (..), mkMessage)

data App = App

mkMessage "App" "values/messages/login" "zh-cn"
