{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module FileCollector.Frontend.Main
  ( jsmMain
  ) where

import           Language.Javascript.JSaddle.Types (JSM)
import           Reflex
import           Reflex.Dom                        hiding (mainWidget)
import           Reflex.Dom.Main                   (mainWidget)

jsmMain :: JSM ()
jsmMain = mainWidget $
    text "Hello world"
