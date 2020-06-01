{-# language TypeApplications #-}

module Figures (
  -- * Constructors for geometric objects
  -- ** Point constructors
  origin
  , aPoint, aLabel
  , point, point'
  , pointOn, projectOn, intersectionPoints
  -- ** Line constructors
  , aLine, aRay, aSegment, oX, oY
  , line, line', ray, ray', segment, segment'
  -- ** Angle constructors
  , anAngle
  , angleBetween
  , adjacent, vertical, complementary
  , bisectrisse
  -- ** Polygon constructors
  , aTriangle, triangle2a
  , aSquare, aRectangle
  , parametricPoly, polarPoly, regularPoly
  -- ** Circle constructors
  , aCircle, circle, circle'
  -- * Modificators
  , at, at', along, along', through, through'
  , translate, scaleAt, scaleXAt, scaleYAt
  , on, normalTo
 ) where

import Data.Complex

import Base
import Point
import Circle
import Line
import Polygon
import Angle
import Decorations
 
------------------------------------------------------------
-- | The origin point
origin :: Point
origin = aPoint

-- | The x-axis
oX = aLine

-- | The y-axis
oY = oX # rotate 90

------------------------------------------------------------

-- | The constructor for a point with given coordinates.
point :: XY -> Point
point = point'

-- | The constructor for a point with given affine coordinates.
point' :: Affine a => a -> Point
point' p = mkPoint (cmp p)

-- | The point on a given curve.
pointOn :: Curve a => a -> Double -> Point
pointOn c t = mkPoint (c @-> t)

-- | Returns a point on the curve which is s normal projection of the given point on the curve.
projectOn :: (Curve c, Affine p) => p -> c -> Point
projectOn p c = pointOn c (p ->@ c)

-- | The template for a point.
aPoint :: Point
aPoint = mkPoint (0 :: CN)

-- | The template for a label.
aLabel :: Label
aLabel = mkLabel origin

-- | Returns a list of intersection points as geomtric figures.
intersectionPoints :: (Intersections a b, Curve a, Curve b) => a -> b -> [Point]
intersectionPoints c1 c2 = point' <$> intersections c1 c2

------------------------------------------------------------

circle' :: Affine a => Double -> a -> Circle
circle' r p = mkCircleRC r (cmp p)

circle :: Double -> XY -> Circle
circle = circle'

-- | The template for a circle.
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

-- | The template for a segment.
aSegment :: Line
aSegment = segment' origin ((1,0) :: XY)

-- | The template for a line.
aLine :: Line
aLine = aSegment `extendAs` Unbound

-- | The template for a ray.
aRay :: Line
aRay = aSegment `extendAs` Semibound

------------------------------------------------------------

-- | The template for an angle with given value
anAngle :: Angular -> Angle
anAngle = Angle 0 0

-- | Returns the angle equal to the angle between thwo lines, located on the first one.
angleBetween :: (IsLine l1,  IsLine l2) => l1 -> l2 -> Angle
angleBetween l1 l2 = anAngle (angle l2 - angle l1)
                     # at' (start l1)
                     # along' l1

adjacent :: Angle -> Angle
adjacent (Angle p s e) = Angle p e (s + 180)

vertical :: Angle -> Angle
vertical = rotate 180

complementary :: Angle -> Angle
complementary (Angle p s e) = Angle p e s

bisectrisse :: IsAngle a => a -> Line
bisectrisse an = aSegment
                 # at' (refPoint an)
                 # along' (pi + angleStart an + 0.5*angleValue an)

------------------------------------------------------------
translate :: Trans a => XY -> a -> a
translate = translate'

scaleAt :: Trans a => XY -> Double -> a -> a
scaleAt = scaleAt'

scaleXAt :: Trans a => XY -> Double -> a -> a
scaleXAt = scaleXAt'

scaleYAt :: Trans a => XY -> Double -> a -> a
scaleYAt = scaleYAt'

rotateAt :: Trans a => XY -> Angular -> a -> a
rotateAt = rotateAt'

at' :: (Affine p, Figure a) => p -> a -> a
at' p fig = superpose (refPoint fig) p fig

at :: Figure a => XY -> a -> a
at = at'

along' :: (Figure f, Affine v, Affine f) => v -> f -> f
along' v l = rotateAt' (refPoint l) (angle v - angle l) l

along :: (Figure a, Affine a) => Double -> a -> a
along d = along' (asDeg d)

-- | Locates an affine object on a given curve agt
-- given parameter and aligns it along a tangent to a curve.
on :: (Figure a, Affine a, Curve c) => c -> Double -> a -> a
on c x = along' (tangent c x) . at' (c @-> x)

-- | Turns and extends the line so that it passes through a given point.
through' :: (Affine p, IsLine l) => p -> l -> l
through' p l = l
               # along' (azimuth p0 p)
               # scaleAt' p0 (distance p0 p / unit l)
  where p0 = start l

-- | A coordinated version of `through`.
through :: IsLine l => XY -> l -> l
through = through'

-- | Turns the line so that it becomes normal to a given curve, pointing towards a curve.
normalTo :: (Curve c, IsLine l) => c -> l -> l
normalTo c l =
  if c `isContaining` s
  then l # along' (normal c (s ->@ c))
  else l # along' (ray' s (s `projectOn` c))
  where s = start l 

------------------------------------------------------------

-- | Constructs a parametric graph as a `Polyline`.
parametricPoly :: (Double -> XY) -> [Double] -> Polygon
parametricPoly f range =
  mkPolyline [ x :+ y | t <- range , let (x,y) = f t ]

-- | Constructs a polar graph as a `Polyline`.
polarPoly :: (Double -> Double) -> [Double] -> Polygon
polarPoly rho range =
  mkPolyline [ mkPolar (rho phi) phi | x <- range
                                     , let phi = 2*pi*x ]

-- | Constructs a regular polygon with given number of sides, enscribed in a unit circle.
regularPoly :: Int -> Polygon
regularPoly n' = rotate 90 $ closePoly $
                 polarPoly (const 1) [0,1/n..1-1/n]
  where n = fromIntegral n'

-- | The template for a square.
aSquare :: Polygon
aSquare = mkPolygon @XY [(0,0),(1,0),(1,1),(0,1)]

-- | The template for a Rectangle.
aRectangle :: Double -> Double -> Polygon
aRectangle a b = aSquare # scaleX a # scaleY b

-- | The template for a triangle.
aTriangle :: Triangle
aTriangle = asCmp 1

-- | Returns a triangle with base 1 and two given angles.
triangle2a :: Angular -> Angular -> Polygon
triangle2a a1 a2 = case intersections r1 r2 of
                    [p] -> mkPolygon [(0,0), (1,0), coord p]
                    [] -> trivialPolygon
  where r1 = aRay # along' a1
        r2 = aRay # at (1,0) # along' (180 - a2)

