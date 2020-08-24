{-# LANGUAGE TemplateHaskell #-}
module CSS
  ( resetCSS
  , revealCSS
    -- Themes
  , beigeCSS
  , blackCSS
  , bloodCSS
  , leagueCSS
  , moonCSS
  , nightCSS
  , serifCSS
  , simpleCSS
  , skyCSS
  , solarizedCSS
  , whiteCSS
    -- highlightjs themes
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

beigeCSS :: Text
beigeCSS = decodeUtf8 $(embedFile "./css/themes/beige.css")

blackCSS :: Text
blackCSS = decodeUtf8 $(embedFile "./css/themes/black.css")

bloodCSS :: Text
bloodCSS = decodeUtf8 $(embedFile "./css/themes/blood.css")

leagueCSS :: Text
leagueCSS = decodeUtf8 $(embedFile "./css/themes/league.css")

moonCSS :: Text
moonCSS = decodeUtf8 $(embedFile "./css/themes/moon.css")

nightCSS :: Text
nightCSS = decodeUtf8 $(embedFile "./css/themes/night.css")

serifCSS :: Text
serifCSS = decodeUtf8 $(embedFile "./css/themes/serif.css")

simpleCSS :: Text
simpleCSS = decodeUtf8 $(embedFile "./css/themes/simple.css")

skyCSS :: Text
skyCSS = decodeUtf8 $(embedFile "./css/themes/sky.css")

solarizedCSS :: Text
solarizedCSS = decodeUtf8 $(embedFile "./css/themes/solarized.css")

whiteCSS :: Text
whiteCSS = decodeUtf8 $(embedFile "./css/themes/white.css")

-- Highlight Plugin

monokaiCSS :: Text
monokaiCSS = decodeUtf8 $(embedFile "./css/monokai.css")
