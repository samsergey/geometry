{-# language TypeApplications #-}
{-# language FlexibleContexts #-}

module Figures (
  -- * Constructors for geometric objects
  -- ** Point constructors
  origin
  , aPoint, aLabel
  , point, point'
  , pointOn, projectOn, intersectionPoints, closestTo
  -- ** Line constructors
  , aLine, aRay, aSegment, oX, oY
  , line, line', ray, ray', segment, segment'
  , extendToLength, extendTo, normalSegment, heightTo
  -- ** Angle constructors
  , anAngle
  , angleBetween
  , supplementary, vertical, reflex
  , bisectrisse
  -- ** Polygon constructors
  , aTriangle, triangle2a
  , aSquare, aRectangle
  , parametricPoly, polarPoly, regularPoly
  -- ** Circle constructors
  , aCircle, circle, circle'
  -- ** Misc
  , linearScale, modularScale
  -- * Modificators
  , at, at', along, along', through, through'
  , translate, scaleAt, scaleXAt, scaleYAt
  , on, normalTo, flipAt
 ) where

import Data.Complex
import Data.List.Extra
import Control.Applicative

import Base
import Point
import Circle
import Line
import Polygon
import Angle
import Decorations
 
------------------------------------------------------------
-- | The origin point. Equivalent to `aPoint`.
origin :: Point
origin = aPoint

-- | The x-axis. Equivalent to `aLine`.
oX = aLine

-- | The y-axis
oY = oX # rotate 90

------------------------------------------------------------

-- | The constructor for a point with given coordinates.
point :: XY -> Point
point = point'

-- | The generalized version of `point`.
point' :: Affine a => a -> Point
point' p = mkPoint (cmp p)

-- | The point on a given curve.
pointOn :: Curve a => a -> Double -> Point
pointOn c t = mkPoint (c @-> t)

-- | Returns a normal projection of the given point on the curve.
projectOn :: (Curve c, Affine p) => p -> c -> Maybe Point
projectOn p c = pointOn c <$> (p ->@? c)

-- | A point at the origin. Equivalent to `origin`.
aPoint :: Point
aPoint = mkPoint (0 :: CN)

-- | A label: the invisible point which coul be labeled using `#:` operator.
aLabel :: Label
aLabel = mkLabel origin

-- | Returns a list of intersection points as `Point` objects.
intersectionPoints :: (Intersections a b, Curve a, Curve b) => a -> b -> [Point]
intersectionPoints c1 c2 = point' <$> intersections c1 c2

-- | Returns a point from a list which is closest to a given one.
closestTo :: (Affine a, Affine b) => a -> [b] -> Maybe b
closestTo p [] = Nothing
closestTo p ps  = Just $ minimumOn (distance p) ps

------------------------------------------------------------

-- | The generalized version of `circle`.
circle' :: Affine a => Double -> a -> Circle
circle' r p = mkCircleRC r (cmp p)

-- | The constructor for a circle with given radius and coordinates.
circle :: Double -> XY -> Circle
circle = circle'

-- | The template for a circle.
aCircle :: Circle
aCircle = circle' 1 origin

------------------------------------------------------------
-- | 
line' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
line' p1 p2 = mkLine (cmp p1, cmp p2)

-- | 
line :: XY -> XY -> Line
line = line'

-- | 
segment' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
segment' p1 p2 = mkSegment (cmp p1, cmp p2)

-- | 
segment :: XY -> XY -> Line
segment = segment'

-- | 
ray' :: (Affine a1, Affine a2) => a1 -> a2 -> Line
ray' p1 p2 = mkRay (cmp p1, cmp p2)

-- | 
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

-- | Returns a line, ray or a segment with given unit, in case of a segment -- with given length.
extendToLength :: Double -> Line -> Line
extendToLength l s = s # through' (paramL s l) 

-- | Returns a segment extended to a closest intersection point with a given curve.
extendTo :: (Curve c, Intersections Line c) => c -> Line -> Line
extendTo c s = case closestTo (start s)  (intersections r c) of
  Nothing -> r
  Just p -> aSegment # at' (start s) # through' p
  where r = s `extendAs` Semibound

-- | Returns a segment normal to a given curve.
heightTo :: (Curve c, Intersections Line c) => c -> Line -> Maybe Line
heightTo c l = extendTo c <$> normalTo c l 
  
------------------------------------------------------------

-- | The template for an angle with given value
anAngle :: Angular -> Angle
anAngle = Angle 0 0

-- | Returns the angle equal to the angle between thwo lines, located on the first one.
angleBetween :: (IsLine l1,  IsLine l2) => l1 -> l2 -> Angle
angleBetween l1 l2 = anAngle (angle l2 - angle l1)
                     # at' (start l1)
                     # along' l1

-- | 
supplementary :: Angle -> Angle
supplementary (Angle p s e) = Angle p e (s + 180)

-- | 
vertical :: Angle -> Angle
vertical = rotate 180

-- | 
reflex :: Angle -> Angle
reflex (Angle p s e) = Angle p e s

-- | 
bisectrisse :: IsAngle a => a -> Line
bisectrisse an = aSegment
                 # at' (refPoint an)
                 # along' (pi + angleStart an + 0.5*angleValue an)

------------------------------------------------------------
-- | Moves an object along given vector.
translate :: Trans f => XY -> (f -> f)
translate = translate'

-- | Scales  an object simmetrically (isotropically) against a given point.
scaleAt :: Trans f => XY -> Double -> (f -> f)
scaleAt = scaleAt'

-- | Scales  an object along x-axis against a given point.
scaleXAt :: Trans f => XY -> Double -> (f -> f)
scaleXAt = scaleXAt'

-- | Scales  an object along y-axis against a given point.
scaleYAt :: Trans f => XY -> Double -> (f -> f)
scaleYAt = scaleYAt'

-- | Rotates  an object  against a given point.
rotateAt :: Trans f => XY -> Angular -> (f -> f)
rotateAt = rotateAt'

-- | Generalized version of  `at` transformer.
at' :: (Affine p, Figure f) => p -> (f -> f)
at' p fig = superpose (refPoint fig) p fig

-- | Moves an object so that it's `refPoint` coinsides with a given one.
at :: Figure f => XY -> (f -> f)
at = at'

-- | Generalized version of  `along` transformer.
along' :: (Figure f, Affine v, Affine f) => v -> (f -> f)
along' v l = rotateAt' (refPoint l) (angle v - angle l) l

-- | Rotates the figure which is `Affine` instance against it's `refPoint` so that it's
-- refference angle (given by `angle`) councides with a given one.
along :: (Figure f, Affine f) => Double -> (f -> f)
along d = along' (asDeg d)

-- | Locates an affine object on a given curve at
-- given parameter and aligns it along a tangent to a curve at this point.
on :: (Figure f, Affine f, Curve c) => c -> Double -> (f -> f)
on c x = along' (tangent c x) . at' (c @-> x)

-- | A generalized version of `through`.
through' :: (Affine p, IsLine l) => p -> (l -> l)
through' p l = l
               # along' (azimuth p0 p)
               # scaleAt' p0 (distance p0 p / unit l)
  where p0 = start l

-- | Turns and extends the line so that it passes through a given point.
through :: IsLine l => XY -> (l -> l)
through = through'

-- | If possible, turns the line so that it becomes normal to a given curve, pointing towards a curve.
normalTo :: (Curve c, IsLine l) => c -> l -> Maybe l
normalTo c l = turn <*> Just l
  where s = start l
        turn = if c `isContaining` s
               then along' . normal c <$> (s ->@? c)
               else along' . ray' s <$> (s `projectOn` c)


-- | Reflects the curve  at a given parameter against the normal, if it exists, or does nothing otherwise.
flipAt :: (Trans c, Curve c) => Double -> c -> c
flipAt x c = case normalSegment c x of
               Just n -> c # reflectAt n
               Nothing -> c

-- | If possible, returns a line segment normal to the curve starting at a given parameter
normalSegment :: Curve c => c -> Double -> Maybe Line
normalSegment c x =
  do p <- paramMaybe c x
     aSegment # at' p # normalTo c
                       
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
triangle2a :: Angular -> Angular -> Triangle
triangle2a a1 a2 = case intersections r1 r2 of
                    [p] -> mkTriangle [(0,0), (1,0), coord p]
                    [] -> trivialTriangle
  where r1 = aRay # along' a1
        r2 = aRay # at (1,0) # along' (180 - a2)

------------------------------------------------------------

-- | Creates a scale as a list of labeled points on a given curve
linearScale :: (Show a, Curve c)
            => (Double -> a) -- ^ labeling function
            -> [Double] -- ^ range of the curve parameter
            -> c -- ^ the curve
            -> [Decorated Point]
linearScale fn rng c = [ pointOn c x
                     #: label (show (fn x)) <> loffs (cmp (normal c x))
                   | x <- rng ]

-- | Creates a circular integer scale, representing modular arithmetics.
modularScale :: (Trans c, Curve c) => Int -> c -> [Decorated Point]
modularScale n = linearScale lf rng
  where n' = fromIntegral n
        rng = (/n') <$> [0..n'-1]
        lf = (`mod` n) . negate . round . (* n')

------------------------------------------------------------

instance Decor Point where
  defaultOptions p = mkOptions
    [ LabelPosition $ cmp p
    , LabelOffset (0 :+ 1)
    , LabelCorner (0, 0)
    , LabelAngle 0
    , Stroke "#444"
    , Fill "red"
    , Thickness "1" ]

instance Decor Label where
  defaultOptions p = mkOptions
    [ LabelPosition $ cmp p
    , LabelOffset 0
    , LabelCorner (0, 0)
    , LabelAngle 0 ]

instance Decor Circle where
  defaultOptions c = mkOptions
    [ LabelPosition $ c @-> 0
    , LabelOffset $ cmp $ normal c 0
    , LabelCorner (-1,0)
    , LabelAngle 0
    , Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

instance Decor Line where
  defaultOptions l = mkOptions
    [ LabelPosition $ l @-> 0.5
    , LabelOffset $ cmp $ normal l 0
    , Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

instance Decor Polygon where
  defaultOptions _ = mkOptions
    [ Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

instance Decor Triangle where
  defaultOptions = defaultOptions . fromTriangle

instance Decor Angle where
  defaultOptions an = mkOptions
    [ Stroke "white"
    , Fill "none"
    , Thickness "1"
    , MultiStroke 1
    , LabelPosition $ refPoint an + scale 27 (cmp (bisectrisse an)) ]
