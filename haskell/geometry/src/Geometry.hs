module Geometry ( module Base
                , module Point
                , module Line
                , module Circle
                , module Polygon
                , module SVG
                , origin
                , at, at', along, reflectAt
                , point, point', pointOn
                , label
                , line, segment, ray
                , aLine, aSegment, aRay
                , circle, circle'
                , parametricPoly, polarPoly, regularPoly
                ) where

import Data.Double.Conversion.Text (toShortest)
import Data.Complex

import Base
import Point
import Label
import Circle
import Line
import Polygon
import SVG
 
------------------------------------------------------------

origin = Point ((0, 0) :: XY)

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
point p = Point (coord p)
 
pointOn :: Curve a => a -> Double -> Point
pointOn c t = Point $ coord $ c `param` t

label :: String -> Label
label s = Label s 0
------------------------------------------------------------

line :: (Affine a1, Affine a2) => a1 -> a2 -> Line
line p1 p2 = Line (cmp p1, cmp p2)

segment :: (Affine a1, Affine a2) => a1 -> a2 -> Line
segment p1 p2 = Segment (cmp p1, cmp p2)

ray :: (Affine a1, Affine a2) => a1 -> a2 -> Line
ray p1 p2 = Ray (cmp p1, cmp p2)

aSegment = segment origin ((1,0) :: XY)
aLine = aSegment `extendAs` Line
aRay = aSegment `extendAs` Ray

------------------------------------------------------------

circle :: Affine a => Double -> a -> Circle
circle r p = mkCircle2 c $ c + (r :+ 0)
  where c = cmp p

------------------------------------------------------------

parametricPoly f range =
  Polyline $ [ x :+ y | t <- range , let (x,y) = f t ]

polarPoly rho range =
  Polyline $ [ mkPolar (rho phi) phi | x <- range
                                     , let phi = 2*pi*x ]

regularPoly :: Int -> Polygon
regularPoly n' = rotate 90 $
                 closePoly $
                 polarPoly (const 1) [0,1/n..1-1/n]
  where n = fromIntegral n'


