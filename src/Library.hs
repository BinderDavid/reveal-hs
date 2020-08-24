{-# LANGUAGE OverloadedStrings #-}
module Library
  ( Slide
  , Theme(..)
  , HighlightTheme(..)
  , RevealConfig(..)
  , slides
  ) where

import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)

import Control.Monad (forM_)
import Data.String (fromString)
import Prelude hiding (head, id, div)

import CSS
import Javascript

-- Configuration

data Theme = Serif
data HighlightTheme = Monokai

data RevealConfig = MkRevealConfig
  { theme :: Theme
  , highlightTheme :: HighlightTheme
  , slidesTitle :: String
  }

type Slide = Html

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
  case theme config of
    Serif -> Text.Blaze.Html5.style $ preEscapedToHtml serifCSS -- id = "theme" ?
  case highlightTheme config of
    Monokai -> Text.Blaze.Html5.style $ preEscapedToHtml monokaiCSS -- id = "highlight-theme" ?

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


slidesToHtml :: [Slide] -> Html
slidesToHtml slides = forM_  slides section

