module Curve where

import Generals

class Curve a where
  point :: a -> Float -> XY
  locus :: a -> XY -> Float
  closed :: a -> Bool
  length :: a -> Float
  tangent :: a -> Float -> Directed
  
  normal :: a -> Float -> Directed
  normal f t = rot90 $ tangent f t

class Curve a => Linear a where
  start :: a -> XY
  vector :: a -> XY
  angle :: a -> Directed
  unit :: a -> Float
