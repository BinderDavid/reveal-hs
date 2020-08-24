{-# LANGUAGE TemplateHaskell #-}
module Javascript
  ( highlightJS
  , mathJS
  , notesJS
  , revealJS
  ) where

import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.FileEmbed (embedFile)

highlightJS :: Text
highlightJS = decodeUtf8 $(embedFile "./javascript/highlight.js")

mathJS :: Text
mathJS = decodeUtf8 $(embedFile "./javascript/math.js")

notesJS :: Text
notesJS = decodeUtf8 $(embedFile "./javascript/notes.js")

revealJS :: Text
revealJS = decodeUtf8 $(embedFile "./javascript/reveal.js")
