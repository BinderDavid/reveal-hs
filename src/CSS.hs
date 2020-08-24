{-# LANGUAGE TemplateHaskell #-}
module CSS
  ( resetCSS
  , revealCSS
  , serifCSS
  , monokaiCSS
  ) where

import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.FileEmbed (embedFile)

resetCSS :: Text
resetCSS = decodeUtf8 $(embedFile "./css/reset.css")

revealCSS :: Text
revealCSS = decodeUtf8 $(embedFile "./css/reveal.css")

-- Themes

serifCSS :: Text
serifCSS = decodeUtf8 $(embedFile "./css/serif.css")

-- Highlight Plugin

monokaiCSS :: Text
monokaiCSS = decodeUtf8 $(embedFile "./css/monokai.css")
