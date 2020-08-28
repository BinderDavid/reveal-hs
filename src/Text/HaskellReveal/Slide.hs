{-# LANGUAGE OverloadedStrings #-}
module Text.HaskellReveal.Slide
  ( Slide(..)
  , TransitionSpeed(..)
  , SimpleTransition(..)
  , Transition(..)
  , mkSlide
  ) where

import Text.Blaze.Html5 (Html, Attribute, (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Internal (attribute)
import Data.String (fromString)

newtype Slide = MkSlide { unSlide :: Html }


mkSlide :: TransitionSpeed -> Transition -> Html -> Slide
mkSlide trs tr h = MkSlide $ H.section ! dataTransition tr ! dataTransitionSpeed trs $ h

------------------------------------------------
-- Transitions

dataTransition :: Transition -> Attribute
dataTransition tr = attribute "data-transition" " data-transition=\"" (fromString . show $ tr)

dataTransitionSpeed :: TransitionSpeed -> Attribute
dataTransitionSpeed trs = attribute "data-transition-speed" " data-transition-speed=\"" (fromString . show $ trs)

data TransitionSpeed
  = Default
  | Fast
  | Slow

instance Show TransitionSpeed where
  show Default = "default"
  show Fast    = "fast"
  show Slow    = "slow"

data SimpleTransition
  = None
  | Fade
  | Slide
  | Convex
  | Concave
  | Zoom

instance Show SimpleTransition where
  show None    = "none"
  show Fade    = "fade"
  show Slide   = "slide"
  show Convex  = "convex"
  show Concave = "concave"
  show Zoom    = "zoom"

data Transition
  = STransition SimpleTransition
  | InOutTransition SimpleTransition SimpleTransition

instance Show Transition where
  show (STransition st) = show st
  show (InOutTransition st st') = show st ++ "-in " ++ show st' ++ "-out"


