{-# language TypeApplications #-}
module Geometry ( module Base
                , module Point
                , module Line
                , module Circle
                , module Polygon
                , module SVG
                , module Geometry
                , module Decorations
                ) where

import Data.Complex

import Base
import Point
import Circle
import Line
import Polygon
import Decorations
import SVG
 
------------------------------------------------------------

origin :: Point
origin = mkPoint (0 :: CN)

point :: XY -> Point
point = point'

point' :: Affine a => a -> Point
point' p = mkPoint (cmp p)

pointOn :: Curve a => a -> Double -> Point
pointOn c t = mkPoint (c.@ t)

aPoint :: Point
aPoint = origin

aLabel :: String -> Decorated Label
aLabel s = mkLabel origin #: label s

------------------------------------------------------------

circle' :: Affine a => Double -> a -> Circle
circle' r p = mkCircle2 c $ c + (r :+ 0)
  where c = cmp p

circle :: Double -> XY -> Circle
circle = circle'

aCircle :: Circle
aCircle = circle' 1 origin

------------------------------------------------------------

line' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
line' p1 p2 = mkLine (cmp p1, cmp p2)

line :: XY -> XY -> Line
line = line'

segment' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
segment' p1 p2 = mkSegment (cmp p1, cmp p2)

segment :: XY -> XY -> Line
segment = segment'

ray' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
ray' p1 p2 = mkRay (cmp p1, cmp p2)

ray :: XY -> XY -> Line
ray = ray'

aSegment, aLine, aRay :: Line
aSegment = segment' origin ((1,0) :: XY)
aLine = aSegment `extendAs` Unbound
aRay = aSegment `extendAs` Semibound

------------------------------------------------------------

parametricPoly :: (Double -> XY) -> [Double] -> Polygon
parametricPoly f range =
  mkPolyline [ x :+ y | t <- range , let (x,y) = f t ]

polarPoly :: (Double -> Double) -> [Double] -> Polygon
polarPoly rho range =
  mkPolyline [ mkPolar (rho phi) phi | x <- range
                                     , let phi = 2*pi*x ]

regularPoly :: Int -> Polygon
regularPoly n' = rotate 90 $ closePoly $
                 polarPoly (const 1) [0,1/n..1-1/n]
  where n = fromIntegral n'

aSquare :: Polygon
aSquare = mkPolygon @XY [(0,0),(1,0),(1,1),(0,1)]

aRectangle :: Double -> Double -> Polygon
aRectangle a b = aSquare # scaleX a # scaleY b

aTriangle :: Polygon
aTriangle = mkPolygon @XY [ (0,0), (1,0)
                          , (cos (pi/3), sin (pi/3))]


