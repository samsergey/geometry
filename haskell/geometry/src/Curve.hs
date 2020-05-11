module Curve where

import Data.Complex

import Generals

class Curve a where
  param :: a -> Number -> XY
  locus :: a -> XY -> Number
  closed :: a -> Bool
  length :: a -> Number
  tangent :: a -> Number -> Directed
  
  normal :: a -> Number -> Directed
  normal f t = 90 + tangent f t

class Curve a => Linear a where
  start :: a -> XY
  end :: a -> XY
  vector :: a -> XY

  unit :: a -> Number
  unit = magnitude . vector

  angle :: a -> Directed
  angle = Vec . vector
  
