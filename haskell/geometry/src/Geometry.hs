module Geometry where

import Data.Double.Conversion.Text (toShortest)
import Data.Complex

gimport Base
import Affine
import Point
import Circle
import Line

------------------------------------------------------------

point :: Affine a => a -> Point
point p = Point (coord p)

origin = Point (0, 0)

pointOn :: Curve a => a -> Double -> Point
pointOn c t = Point $ coord $ c `param` t

------------------------------------------------------------

line :: (Affine a1, Affine a2) => a1 -> a2 -> Line
line p1 p2 = Line (cmp p1) (cmp p2)

------------------------------------------------------------

circle :: Affine a => Double -> a -> Circle
circle r p = mkCircle2 c $ c + (r :+ 0)
  where c = cmp p


------------------------------------------------------------

