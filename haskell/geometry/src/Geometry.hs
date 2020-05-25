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

------------------------------------------------------------

-- | The transformation with shifted origin.
transformAt :: (Trans a, Affine p) => p -> (a -> a) -> a -> a
transformAt p t = translate xy . t . translate (-xy)
  where xy = cmp p

-- | The translation of an object.
translate :: (Trans a, Affine p) => p -> a -> a
translate = transform . translateT . cmp

-- | The translation leading to a superposition of tho points.
superpose :: (Trans a, Affine p1, Affine p2) => p1 -> p2 -> a -> a
superpose p1 p2 = translate (cmp p2 - cmp p1)

-- | The isotropic scaling of an object.
scale :: Trans a => Double -> a -> a
scale s = transform (scaleT s s)

-- | The scaling of an object in x-direction.
scaleX :: Trans a => Double -> a -> a
scaleX s = transform (scaleT s 1)

-- | The scaling of an object in y-direction.
scaleY :: Trans a => Double -> a -> a
scaleY s = transform (scaleT 1 s)

-- | The isotropic scaling of an object against a given point.
scaleAt :: (Trans a, Affine p) => p -> Double -> a -> a
scaleAt p s = transformAt p (scale s)

-- | The isotropic scaling of an object in x-direction against a given point.
scaleXAt :: (Trans a, Affine p) => p -> Double -> a -> a
scaleXAt p s = transformAt p (scaleX s)

-- | The isotropic scaling of an object in y-direction against a given point.
scaleYAt :: (Trans a, Affine p) => p -> Double -> a -> a
scaleYAt p s = transformAt p (scaleY s)

-- | The rotation of an object against the origin.
rotate :: Trans a => Angular -> a -> a
rotate = transform . rotateT . rad

-- | The rotation of an object against a given point.
rotateAt :: (Trans a, Affine p) => p -> Angular -> a -> a
rotateAt p a = transformAt p (rotate a)

-- | The reflection of an object against the direction passing through the origin.
reflect :: Trans a => Angular -> a -> a
reflect d = transform $ reflectT $ rad d

at' :: (Affine p, Figure a) => p -> a -> a
at' p fig = superpose (refPoint fig) p fig

at :: Figure a => XY -> a -> a
at = at'

along' :: (Figure f, Affine v, Affine f) => v -> f -> f
along' v l = rotateAt (refPoint l) (angle v - angle l) l

along :: (Figure a, Affine a) => Double -> a -> a
along d = along' (asDeg d)

reflectAt :: (Curve l, Affine l, Trans a) => l -> a -> a
reflectAt l = transformAt (l.@ 0) (reflect (angle l))

------------------------------------------------------------

