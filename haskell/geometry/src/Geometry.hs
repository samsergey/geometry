module Geometry where

import Generals
import Curve
import Transform

data Point = Point XY deriving Show

instance Trans Point where
  transform t (Point (x,y)) = Point (transformXY t (x,y))
