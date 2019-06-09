{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module FileCollector.Frontend.Message
  ( renderMsgDyn
  , renderMsg
  , Login(..)
  , LoginMessage(..)
  , TopBar(..)
  , TopBarMessage(..)
  ) where

import FileCollector.Frontend.Message.Prelude

data Login = Login
mkMessage "Login" "values/messages/login" "zh-CN"

data TopBar = TopBar
mkMessage "TopBar" "values/messages/topbar" "zh-CN"
