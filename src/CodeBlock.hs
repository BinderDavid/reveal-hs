{-# LANGUAGE OverloadedStrings #-}
module CodeBlock where

import Data.String ( fromString )
import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (attribute)

dataTrim :: Attribute
dataTrim = attribute "data-trim" " data-trim=\"" ""

dataNoEscape :: Attribute
dataNoEscape = attribute "data-noescape" " data-noescape=\"" ""

dataLineNumbers :: AttributeValue -> Attribute
dataLineNumbers = attribute "data-line-numbers" " data-line-numbers=\""


codeBlock :: String -> [String] -> Html
codeBlock lineNumbers snippet = pre $ code ! class_ "haskell" ! dataTrim ! dataNoEscape ! dataLineNumbers (fromString lineNumbers) $ sequence_ (fromString <$> snippet)
