{-# LANGUAGE OverloadedStrings #-}
module Slide
  ( Slide(..)
  , createSlide
  ) where

import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Internal (attribute)

import Control.Monad (forM_)
import Data.String (fromString)
import Prelude hiding (head, id, div)


newtype Slide = MkSlide { unSlide :: Html }


createSlide :: Html -> Slide
createSlide h = MkSlide $ section ! dataTransition (STransition Zoom) ! dataTransitionSpeed Slow $ h

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


