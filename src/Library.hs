{-# LANGUAGE OverloadedStrings #-}
module Library
  ( Slide
  , slides
  ) where

import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Pretty

import Control.Monad (forM_)
import Prelude hiding (head, id, div)

import CSS
import Javascript

type Slide = Html

slides :: [Slide] -> Html
slides slides = docTypeHtml $ do
  head slidesHead
  body (slidesBody slides)

slidesHead :: Html
slidesHead = do
  meta ! charset "utf-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
  title "TITLE"
  Text.Blaze.Html5.style $ preEscapedToHtml resetCSS
  Text.Blaze.Html5.style $ preEscapedToHtml revealCSS
  Text.Blaze.Html5.style $ preEscapedToHtml serifCSS -- id = "theme" ?
  Text.Blaze.Html5.style $ preEscapedToHtml monokaiCSS -- id = "highlight-theme" ?

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

