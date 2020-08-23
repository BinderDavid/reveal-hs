module Main where

import Library

main :: IO ()
main = do
  putStrLn "Writing to file index.html"
  writeFile "index.html" renderedSlides

