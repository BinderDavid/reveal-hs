{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Blaze.Html
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Internal (attribute)

import Control.Monad (forM_)
import Prelude hiding (head, id, div)

import Library

dataTrim :: Attribute
dataTrim = attribute "data-trim" " data-trim=\"" ""
dataNoEscape :: Attribute
dataNoEscape = attribute "data-noescape" " data-noescape=\"" ""
dataLineNumbers :: AttributeValue -> Attribute
dataLineNumbers = attribute "data-line-numbers" " data-line-numbers=\""

slide1, slide2, slide3, slide4 :: Slide
slide1 = h2 $ "Title Slide"

slide2 = do
  h3 $ "Slide 2"
  ul $ do
    li $ "point 1"
    li $ "point 2"

slide3 = do
  h3 $ "Code"
  pre $ code ! class_ "haskell" ! dataTrim ! dataNoEscape ! dataLineNumbers "1-2" $ do
    "main :: IO ()"
    "main = putStrLn \"Hello World\""

slide4 = do
  h3 $ "Test"
  "hello"

renderedSlides :: String
renderedSlides = renderHtml $ slides [slide1, slide2, slide3, slide4]

main :: IO ()
main = do
  putStrLn "Writing to file index.html"
  writeFile "index.html" renderedSlides

