{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze.Html
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Pretty

import Control.Monad (forM_)
import Prelude hiding (head, id, div)

import Text.HaskellReveal.Slide
import Text.HaskellReveal.Presentation
import Text.HaskellReveal.CodeBlock

-- We choose a theme for the presentation, a theme for
-- the code highlighter, and a title for the slides.
config :: RevealConfig
config = MkRevealConfig
  { theme = Serif
  , highlightTheme = Monokai
  , slidesTitle = "MySlides"
  }

titleSlide :: Slide
titleSlide = createSlide $ do
  h2 $ "Title Slide"

listSlide :: Slide
listSlide = createSlide $ do
  h3 $ "Slide 2"
  ul $ do
    li $ "point 1"
    li $ "point 2"

codeSlide :: Slide
codeSlide = createSlide $ do
  h3 $ "Code"
  codeBlock "1-2"
    [ "main :: IO ()"
    , "main = putStrLn \"Hello World\""
    ]

goodbyeSlide :: Slide
goodbyeSlide = createSlide $ do
  h3 $ "Test"
  "Goodbye"

slides :: [Slide]
slides = [ titleSlide
         , listSlide
         , codeSlide
         , goodbyeSlide
         ]

-- Running the program will write the presentation to disk as "index.html"
main :: IO ()
main = mkPresentation slides config "index.html"

