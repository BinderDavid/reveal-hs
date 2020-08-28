{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze.Html
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Pretty

import Control.Monad (forM_)
import Prelude hiding (head, id, div)

import Slide
import Presentation
import CodeBlock

config :: RevealConfig
config = MkRevealConfig
  { theme = Serif
  , highlightTheme = Monokai
  , slidesTitle = "MySlides"
  }

slide1 :: Slide
slide1 = createSlide $ h2 $ "Title Slide"

slide2 :: Slide
slide2 = createSlide $ do
  h3 $ "Slide 2"
  ul $ do
    li $ "point 1"
    li $ "point 2"

slide3 :: Slide
slide3 = createSlide $ do
  h3 $ "Code"
  codeBlock "1-2"
    [ "main :: IO ()"
    , "main = putStrLn \"Hello World\""
    ]

slide4 :: Slide
slide4 = createSlide $ do
  h3 $ "Test"
  "hello"

mySlides :: [Slide]
mySlides = [slide1, slide2, slide3, slide4]

renderedSlides :: String
renderedSlides = renderHtml $ slides config mySlides

main :: IO ()
main = do
  putStrLn "Writing to file index.html"
  writeFile "index.html" renderedSlides

