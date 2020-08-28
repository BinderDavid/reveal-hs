{-# LANGUAGE OverloadedStrings #-}
module Text.HaskellReveal.Presentation
  ( Theme(..)
  , HighlightTheme(..)
  , RevealConfig(..)
  , mkPresentation
  ) where

import Text.Blaze.Html5 (Html, (!), preEscapedToHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Control.Monad (forM_)
import Data.String (fromString)

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

data HighlightTheme
  = Monokai

data RevealConfig = MkRevealConfig
  { theme :: Theme
  , highlightTheme :: HighlightTheme
  , slidesTitle :: String
  }

mkPresentation :: [Slide] -> RevealConfig -> FilePath -> IO ()
mkPresentation slides config fp = do
  let renderedSlides = H.docTypeHtml $ do
          H.head (slidesHead config)
          H.body (slidesBody slides)
  writeFile fp (renderHtml renderedSlides)

slidesHead :: RevealConfig -> Html
slidesHead config = do
  H.meta ! A.charset "utf-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
  H.title (fromString . slidesTitle $ config)
  H.style $ preEscapedToHtml resetCSS
  H.style $ preEscapedToHtml revealCSS
  H.style $ preEscapedToHtml (case theme config of -- id = "theme" ?
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
  H.style $ preEscapedToHtml (case highlightTheme config of -- id = "highlight-theme" ?
                                 Monokai -> monokaiCSS)

slidesBody :: [Slide] -> Html
slidesBody slides = do
  H.div ! A.class_ "reveal" $ H.div ! A.class_ "slides" $ forM_ slides unSlide

  H.script $ preEscapedToHtml revealJS
  H.script $ preEscapedToHtml notesJS
  H.script $ preEscapedToHtml highlightJS
  H.script $ preEscapedToHtml mathJS

  H.script $ preEscapedToHtml $ unlines
    [ "Reveal.initialize({"
    , "  hash: true,"
    , "  math: {"
    , "     mathjax: 'https://cdn.jsdelivr.net/gh/mathjax/mathjax@2.7.8/MathJax.js',"
    , "     config: 'TeX-AMS_HTML-full',"
    , "  },"
    , "  plugins: [ RevealHighlight, RevealNotes, RevealMath ]"
    , "});"
    ]
