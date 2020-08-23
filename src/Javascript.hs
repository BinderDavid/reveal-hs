{-# LANGUAGE TemplateHaskell #-}
module Javascript where

import qualified Data.ByteString as BS
import Data.Text
import Data.Text.Encoding
import Data.FileEmbed (embedFile)


highlightJS :: Text
highlightJS = decodeUtf8 $(embedFile "./javascript/highlight.js")

mathJS :: Text
mathJS = decodeUtf8 $(embedFile "./javascript/math.js")

notesJS :: Text
notesJS = decodeUtf8 $(embedFile "./javascript/notes.js")

revealJS :: Text
revealJS = decodeUtf8 $(embedFile "./javascript/reveal.js")
