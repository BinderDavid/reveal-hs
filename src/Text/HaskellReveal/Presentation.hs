{-# LANGUAGE OverloadedStrings #-}
module Text.HaskellReveal.Presentation where

import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Internal (attribute)

import Control.Monad (forM_)
import Data.String (fromString)
import Prelude hiding (head, id, div)

import Text.HaskellReveal.Slide
import Text.HaskellReveal.CSS
import Text.HaskellReveal.Javascript

-- Configuration

data Theme
  = Beige
  | Black
  | Blood
  | League
  | Moon
  | Night
  | Serif
  | Simple
  | Sky
  | Solarized
  | White

data HighlightTheme = Monokai

data RevealConfig = MkRevealConfig
  { theme :: Theme
  , highlightTheme :: HighlightTheme
  , slidesTitle :: String
  }

slidesToHtml :: [Slide] -> Html
slidesToHtml slides = forM_  slides unSlide

slides :: RevealConfig -> [Slide] -> Html
slides config slides = docTypeHtml $ do
  head (slidesHead config)
  body (slidesBody slides)

slidesHead :: RevealConfig -> Html
slidesHead config = do
  meta ! charset "utf-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
  title (fromString . slidesTitle $ config)
  Text.Blaze.Html5.style $ preEscapedToHtml resetCSS
  Text.Blaze.Html5.style $ preEscapedToHtml revealCSS
  Text.Blaze.Html5.style $ preEscapedToHtml (case theme config of -- id = "theme" ?
                                               Beige     -> beigeCSS
                                               Black     -> blackCSS
                                               Blood     -> bloodCSS
                                               League    -> leagueCSS
                                               Moon      -> moonCSS
                                               Night     -> nightCSS
                                               Serif     -> serifCSS
                                               Simple    -> simpleCSS
                                               Sky       -> skyCSS
                                               Solarized -> solarizedCSS
                                               White     -> whiteCSS)
  Text.Blaze.Html5.style $ preEscapedToHtml (case highlightTheme config of -- id = "highlight-theme" ?
                                               Monokai -> monokaiCSS)

slidesBody :: [Slide] -> Html
slidesBody slides = do
  div ! class_ "reveal" $ div ! class_ "slides" $ slidesToHtml slides

  script $ preEscapedToHtml revealJS
  script $ preEscapedToHtml notesJS
  script $ preEscapedToHtml highlightJS
  script $ preEscapedToHtml mathJS

  script $ preEscapedToHtml $ unlines
    [ "Reveal.initialize({"
    , "  hash: true,"
    , "  math: {"
    , "     mathjax: 'https://cdn.jsdelivr.net/gh/mathjax/mathjax@2.7.8/MathJax.js',"
    , "     config: 'TeX-AMS_HTML-full',"
    , "  },"
    , "  plugins: [ RevealHighlight, RevealNotes, RevealMath ]"
    , "});"
    ]
