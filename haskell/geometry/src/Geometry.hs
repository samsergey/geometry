{-# Language TypeApplications #-}
module Geometry where

import Data.Double.Conversion.Text (toShortest)
import Data.Complex

import Base
import Affine
import Figure
import Point
import Circle
import Line

------------------------------------------------------------

paperSize = 50

origin = Point ((0, 0) :: XY)
plane = circle (paperSize/2) origin

------------------------------------------------------------

fig `at` p = superpose (refPoint fig) p fig

along l1 l2 = rotateAt (refPoint l1) (angle l2 - angle l1) l1


------------------------------------------------------------

point :: Affine a => a -> Point
point p = Point (coord p)
 
pointOn :: Curve a => a -> Double -> Point
pointOn c t = Point $ coord $ c `param` t

------------------------------------------------------------

line :: (Affine a1, Affine a2) => a1 -> a2 -> Line
line p1 p2 = Line (cmp p1, cmp p2)

segment :: (Affine a1, Affine a2) => a1 -> a2 -> Line
segment p1 p2 = Segment (cmp p1, cmp p2)

ray :: (Affine a1, Affine a2) => a1 -> a2 -> Line
ray p1 p2 = Ray (cmp p1, cmp p2)

reflectAt :: (Trans a) => Line -> a -> a
reflectAt l = transformAt (l <@ 0) (reflect (angle l))

aSegment = segment origin ((1,0) :: XY)
aLine = aSegment `extendAs` Line
aRay = aSegment `extendAs` Ray


------------------------------------------------------------

circle :: Affine a => Double -> a -> Circle
circle r p = mkCircle2 c $ c + (r :+ 0)
  where c = cmp p


------------------------------------------------------------

