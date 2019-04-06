{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module FileCollector.Frontend.Main
  ( jsmMain
  ) where

import           Control.Applicative               ((<$>), (<*>))
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Text                         (Text, pack, unpack)
import           Language.Javascript.JSaddle.Types (JSM)
import           Reflex
import           Reflex.Dom                        hiding (mainWidgetWithHead)
import           Reflex.Dom.Main                   (mainWidgetWithHead)
import           Text.Read                         (readMaybe)

jsmMain :: JSM ()
jsmMain = mainWidgetWithHead headElement $ do
  el "h1" $ text "Simple Calculator"
  el "div" $ do
    nx <- numberInput
    d <- dropdown Times (constDyn ops) def
    ny <- numberInput
    let values = zipDynWith (,) nx ny
        result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
        resultText = fmap (pack . show) result
    text " = "
    dynText resultText

headElement :: MonadWidget t m => m ()
headElement =
  elAttr "link"
    [ ("rel", "stylesheet")
    , ("type", "text/css")
    , ("href", "static/simple.css")
    ] $ blank

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let initAttrs = ("type" =: "number") <> (style False)
      color error = if error then "red" else "green"
      style error = "style" =: ("border-color: " <> color error)
      styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
      styleChange result = case result of
        (Just _)  -> fmap Just (style False)
        (Nothing) -> fmap Just (style True)

  rec
    n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrEv
    let result = fmap (readMaybe . unpack) $ _inputElement_value n
        modAttrEv  = fmap styleChange (updated result)
  return result

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
            Plus   -> (+)
            Minus  -> (-)
            Times  -> (*)
            Divide -> (/)
