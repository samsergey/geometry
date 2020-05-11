module Curve where

import Generals

class Curve a where
  point :: a -> Number -> XY
  locus :: a -> XY -> Number
  closed :: a -> Bool
  length :: a -> Number
  tangent :: a -> Number -> Directed
  
  normal :: a -> Number -> Directed
  normal f t = 90 + tangent f t

class Curve a => Linear a where
  start :: a -> XY
  vector :: a -> XY
  angle :: a -> Directed
  unit :: a -> Number
