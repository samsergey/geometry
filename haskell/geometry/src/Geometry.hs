module Geometry ( module Base
                , module Point
                , module Line
                , module Circle
                , module Polygon
                , module SVG
                , origin, aPoint, aLabel
                , at, at', along, reflectAt
                , point, point', pointOn
                , line, segment, ray
                , aLine, aSegment, aRay
                , circle, circle'
                , parametricPoly, polarPoly, regularPoly
                , aTriangle, aSquare
                ) where

import Data.Double.Conversion.Text (toShortest)
import Data.Complex

import Base
import Point
import Circle
import Line
import Polygon
import SVG
 
------------------------------------------------------------

origin = mkPoint 0

aPoint = origin

aLabel s = mkLabel 0 <| label s

point' = point @XY

circle' = circle @XY

at' :: Figure a => XY -> a -> a
at' = at

------------------------------------------------------------

at p fig = superpose (refPoint fig) p fig

along l2 l1 = rotateAt (refPoint l1) (angle l2 - angle l1) l1

reflectAt :: (Trans a) => Line -> a -> a
reflectAt l = transformAt (l .@ 0) (reflect (angle l))

------------------------------------------------------------

point :: Affine a => a -> Point
point p = mkPoint (cmp p)

pointOn :: Curve a => a -> Double -> Point
pointOn c t = mkPoint $ c `param` t

------------------------------------------------------------

line :: (Affine a1, Affine a2) => a1 -> a2 -> Line
line p1 p2 = mkLine (cmp p1, cmp p2)

segment :: (Affine a1, Affine a2) => a1 -> a2 -> Line
segment p1 p2 = mkSegment (cmp p1, cmp p2)

ray :: (Affine a1, Affine a2) => a1 -> a2 -> Line
ray p1 p2 = mkRay (cmp p1, cmp p2)

aSegment = segment origin ((1,0) :: XY)
aLine = aSegment `extendAs` Line
aRay = aSegment `extendAs` Ray

------------------------------------------------------------

circle :: Affine a => Double -> a -> Circle
circle r p = mkCircle2 c $ c + (r :+ 0)
  where c = cmp p

aCircle = circle 1 origin

------------------------------------------------------------

parametricPoly f range =
  Polyline mempty [ x :+ y | t <- range , let (x,y) = f t ]

polarPoly rho range =
  Polyline mempty [ mkPolar (rho phi) phi | x <- range
                                             , let phi = 2*pi*x ]

regularPoly :: Int -> Polygon
regularPoly n' = rotate 90 $
                 closePoly $
                 polarPoly (const 1) [0,1/n..1-1/n]
  where n = fromIntegral n'

aSquare = mkPolygon @XY [(0,0),(1,0),(1,1),(0,1)]
aTriangle = mkPolygon @XY [(0,0),(1,0),(cos (pi/3), sin (pi/3))]
