{-# LANGUAGE OverloadedStrings #-}
module Library where

import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Pretty

import Prelude hiding (head, id, div)

renderedSlides :: String
renderedSlides = renderHtml slides

slides :: Html
slides = docTypeHtml $ do
  head slidesHead
  body slidesBody

slidesHead :: Html
slidesHead = do
  meta ! charset "utf-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
  title "TITLE"
  link ! rel "stylesheet" ! href "dist/reset.css"
  link ! rel "stylesheet" ! href "dist/reveal.css"
  link ! rel "stylesheet" ! href "dist/theme/serif.css" ! id "theme"
  link ! rel "stylesheet" ! href "plugin/highlight/monokai.css" ! id "highlight-theme"

slidesBody :: Html
slidesBody = do
  div ! class_ "reveal" $ div ! class_ "slides" $ slidesContent
  script ! src "dist/reveal.js" $ mempty
  script ! src "plugin/notes/notes.js" $ mempty
  script ! src "plugin/markdown/markdown.js" $ mempty
  script ! src "plugin/highlight/highlight.js" $ mempty
  script ! src "plugin/math/math.js" $ mempty
  script $ preEscapedToHtml $ unlines
    [ "Reveal.initialize({"
    , "  hash: true,"
    , "  math: {"
    , "     mathjax: 'https://cdn.jsdelivr.net/gh/mathjax/mathjax@2.7.8/MathJax.js',"
    , "     config: 'TeX-AMS_HTML-full',"
    , "  },"
    , "  plugins: [ RevealMarkdown, RevealHighlight, RevealNotes, RevealMath ]"
    , "});"
    ]

slidesContent :: Html
slidesContent = mempty
