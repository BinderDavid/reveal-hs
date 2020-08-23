{-# LANGUAGE TemplateHaskell #-}
module CSS where

import qualified Data.ByteString as BS
import Data.Text
import Data.Text.Encoding
import Data.FileEmbed (embedFile)

resetCSS :: Text
resetCSS = decodeUtf8 $(embedFile "./css/reset.css")

revealCSS :: Text
revealCSS = decodeUtf8 $(embedFile "./css/reveal.css")


-- Themes

serifCSS :: Text
serifCSS = decodeUtf8 $(embedFile "./css/serif.css")

-- Plugins

monokaiCSS :: Text
monokaiCSS = decodeUtf8 $(embedFile "./css/monokai.css")
