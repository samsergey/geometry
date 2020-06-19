{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}

module Geometry.Figures (
  -- * Constructors for geometric objects
  -- ** Point constructors
  origin
  , aPoint, aLabel
  , point, point'
  , pointOn, projectOn, intersectionPoints, closestTo
  -- ** Line constructors
  , aLine, aRay, aSegment, oX, oY
  , line, line', ray, ray', segment, segment'
  , extendToLength, extendTo, normalSegment, heightTo, clipBy
  -- ** Angle constructors
  , anAngle
  , angleBetween, angleWithin, bisectrisse
  , supplementary, vertical, reflex
  -- ** Polygon constructors
  , aTriangle, triangle2a
  , aSquare, aRectangle, space
  , parametricPoly, polarPoly, regularPoly
  -- ** Circle constructors
  , aCircle, circle, circle'
  -- ** Misc
  , linearScale, modularScale
  -- * Modificators
  , at, at', along, along', through, through'
  , translate, scaleAt, scaleXAt, scaleYAt, scaleFig
  , on, normalTo, flipAt
  , vertexAngle, height
 ) where

import Data.Complex
import Data.List.Extra (minimumOn, sortOn)
import Control.Applicative

import Geometry.Base
import Geometry.Point
import Geometry.Circle
import Geometry.Line
import Geometry.Angle
import Geometry.Polygon
import Geometry.Intersections
import Geometry.Decorations
 
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
pointOn :: Manifold CN a => a -> Double -> Point
pointOn c t = mkPoint (c @-> t)

-- | Returns a normal projection of the given point on the curve.
projectOn :: (Manifold CN c, Affine p) => p -> c -> Maybe Point
projectOn p c = pointOn c <$> (cmp p ->@? c)

-- | A point at the origin. Equivalent to `origin`.
aPoint :: Point
aPoint = mkPoint (0 :: CN)

-- | A label: the invisible point which coul be labeled using `#:` operator.
aLabel :: Label
aLabel = mkLabel origin

-- | Returns a list of intersection points as `Point` objects.
intersectionPoints :: ( Curve a, Curve b, Intersections a b )
                   => a -> b -> [Point]
intersectionPoints c1 c2 = point' <$> intersections c1 c2

-- | Returns a point from a list which is closest to a given one.
closestTo :: (Affine a, Affine b) => a -> [b] -> Maybe b
closestTo p [] = Nothing
closestTo p ps  = Just $ minimumOn (distance p) ps

------------------------------------------------------------

-- | The generalized version of `circle`.
circle' :: Affine a => Double -> a -> Circle
circle' r p = mkCircle r (cmp p)

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
segment' :: (Affine a1, Affine a2) => a1 -> a2 -> Segment
segment' p1 p2 = mkSegment (cmp p1, cmp p2)

-- | 
segment :: XY -> XY -> Segment
segment = segment'

-- | 
ray' :: (Affine a1, Affine a2) => a1 -> a2 -> Ray
ray' p1 p2 = mkRay (cmp p1, cmp p2)

-- | 
ray :: XY -> XY -> Ray
ray = ray'

-- | The template for a segment.
aSegment :: Segment
aSegment = asCmp 1

-- | The template for a line.
aLine :: Line
aLine = asCmp 1

-- | The template for a ray.
aRay :: Ray
aRay = asCmp 1

-- | Returns a line, ray or a segment with given unit, in case of a segment -- with given length.
extendToLength :: Double -> Segment -> Segment
extendToLength l s = s # through' (paramL (asLine s) l)

-- | Returns a segment extended to a closest intersection point with a given curve.
extendTo :: (Curve c, Intersections Ray c)
         => c -> Segment -> Maybe Segment
extendTo c s = extend <$> closestTo (start s) (intersections (asRay s) c)
  where extend p = s # through' p

-- | Returns a segment normal to a given curve starting at given point.
heightTo :: (Affine p, Curve c, Intersections Ray c)
         => c -> p -> Maybe Segment
heightTo c p = (aSegment # at' p # normalTo c) >>= extendTo c

-- | Returns a list of segments as a result of clipping the line
-- by a closed curve.
clipBy :: (Linear l, Intersections l c, Figure c, ClosedCurve c)
       => l -> c -> [Segment]
clipBy l c = filter internal $ Segment <$> zip ints (tail ints) 
  where
    ints = sortOn (project l) $ intersections l c <> ends
    internal s = c `isEnclosing` (s @-> 0.5)
    ends = (l @->) <$> bounds l
  

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

-- | Scales  an object simmetrically (isotropically) against a given point.
scaleFig :: (Figure f, Trans f) => Double -> (f -> f)
scaleFig s f = scaleAt' (refPoint f) s f


-- | Rotates  an object  against a given point.
rotateAt :: Trans f => XY -> Direction -> (f -> f)
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
through' :: (Affine p, Linear l) => p -> (l -> l)
through' p l = l
               # along' (azimuth p0 p)
               # scaleAt' p0 (distance p0 p / unit l)
  where p0 = start l

-- | Turns and extends the line so that it passes through a given point.
through :: Linear l => XY -> (l -> l)
through = through'

-- | If possible, turns the line so that it becomes normal to a given curve, pointing towards a curve.
normalTo :: (Curve c, Linear l) => c -> l -> Maybe l
normalTo c l = turn <*> Just l
  where s = start l
        turn = if c `isContaining` s
               then along' . normal c <$> (s ->@? c)
               else along' . ray' s <$> (s `projectOn` c)


-- | Reflects the curve  at a given parameter against the normal, if it exists, or does nothing otherwise.
flipAt :: (Curve c) => Double -> c -> c
flipAt x c = case normalSegment c x of
               Just n -> c # reflectAt n
               Nothing -> c

-- | If possible, returns a line segment normal to the curve starting at a given parameter
normalSegment :: Curve c => c -> Double -> Maybe Segment
normalSegment c x =
  do p <- paramMaybe c x
     aSegment # at' p # normalTo c

------------------------------------------------------------

-- $ang| The `Angle` mark looks like a small labeled arc and two segments.
--
-- >>> anAngle 30
-- <Angle 30 (0.0, 0.0)>
--
-- > writeSVG 400 "figs/angle1.svg" $
-- >   let t = triangle2a 30 60
-- >      a1 = anAngle 30 #: "#" <> loffs ((-1):+1)
-- >      a2 = anAngle 90 # on (side t 2) 0 #: "#"
-- >      a3 = vertexAngle t 1 #: "#"
-- >  in t <+> a1 <+> a2 <+> a3
--
-- <<figs/angle1.svg>>
--
-- Angle is a `Manifold Direction` instance:
--
-- >>> Angle 0 0 90 @-> 0.5
-- 45°
-- >>>  Angle 0 0 90 @-> 0
-- 0°
-- >>> Angle 0 0 90 @-> 1
-- 90°
--
-- >>> 45 ->@ Angle 0 0 90
-- 0.5
-- >>> 180 ->@ Angle 0 0 90
-- 2.0
-- >>> 0 ->@ Angle 0 10 30
-- 17.5$ang

-- | The template for an angle with given value.
anAngle :: Direction -> Angle
anAngle = Angle 0 0.01

-- | Returns the angle equal to the angle between two lines, located on the first one.
angleBetween :: (Linear l1,  Linear l2) => l1 -> l2 -> Angle
angleBetween l1 l2 = anAngle (angle l2 - angle l1)
                     # at' (start l1)
                     # along' l1

-- | Returns the Angle mark for the angle, formed by three points.
angleWithin :: (Affine a1, Affine a2, Affine a3)
            => a1 -> a2 -> a3 -> Angle
angleWithin p1 p2 p3 = angleBetween (ray' p2 p1) (ray' p2 p3)

-- | Returns the Angle mark for the angle at given polyline vertex.
vertexAngle :: PiecewiseLinear p => p -> Int -> Angle
vertexAngle p n = angleWithin p3 p2 p1 # innerAngle
  where p1 = vertex p (n-1)
        p2 = vertex p n
        p3 = vertex p (n+1)

-- | Returns a ray, representng the bisectrisse of a given angle.
bisectrisse :: Angular a => a -> Ray
bisectrisse an = aRay # at' (refPoint an) # along' (an @-> 0.5)

-- | The supplementary angle.
--
-- > let a = anAngle 60 #: "a"
-- >     b = a # supplementary #: "b"
-- > in aRay <+> aRay # rotate 60 <+> a <+> b
--
-- <<figs/angle2.svg>>
-- 
supplementary :: Angular an => an -> an
supplementary = asAngle . (\(Angle p s e) -> Angle p e (s + 180)) . toAngle

-- | The vertical angle.
--
-- > let a = anAngle 60 #: "a"
-- >     b = a # vertical #: "b"
-- > in aRay <+> aRay # rotate 60 <+> a <+> b
--
-- <<figs/angle3.svg>>
-- 
vertical :: Angular an => an -> an
vertical = rotate 180

-- | The reflex angle.
--
-- > let a = anAngle 60 #: "a"
-- >     b = a # reflex #: "b"
-- > in aRay <+> aRay # rotate 60 <+> a <+> b
--
-- <<figs/angle4.svg>>
-- 
reflex :: Angular an => an -> an
reflex an = asAngle $ Angle (refPoint an) e s
  where s = angleStart an
        e = angleEnd an


-- | Returns the inner angle (less than `pi`) for a given one.
innerAngle :: Angular an => an -> an
innerAngle an | deg (angleValue an) > 180 = reflex an
              | otherwise = an

------------------------------------------------------------

-- | Constructs a parametric graph as a `Polyline`.
parametricPoly :: (Double -> XY) -> [Double] -> Polyline
parametricPoly f range =
  mkPolyline [ x :+ y | t <- range , let (x,y) = f t ]

-- | Constructs a polar graph as a `Polyline`.
polarPoly :: (Double -> Double) -> [Double] -> Polyline
polarPoly rho range =
  mkPolyline [ mkPolar (rho phi) phi | x <- range
                                     , let phi = 2*pi*x ]

-- | Constructs a regular polygon with given number of sides, enscribed in a unit circle.
regularPoly :: Int -> Polygon
regularPoly n' = rotate 90 $ closePolyline $
                 polarPoly (const 1) [0,1/n..1-1/n]
  where n = fromIntegral n'

-- | The template for a triangle.
aTriangle :: Triangle
aTriangle = asCmp 1

-- | The template for a square.
aSquare :: Rectangle
aSquare = asCmp 1

space :: Double -> Decorated Rectangle
space a = aSquare # scale a #: invisible

-- | The template for a Rectangle.
aRectangle :: Double -> Double -> Rectangle
aRectangle a b = aSquare # scaleX a . scaleY b

-- | Returns a triangle with base 1 and two given angles.
triangle2a :: Direction -> Direction -> Triangle
triangle2a a1 a2 = case intersections r1 r2 of
                    [p] -> mkTriangle [(0,0), (1,0), coord p]
                    [] -> mkTriangle @XY [(0,0), (1,0), (0,0)]
  where r1 = aRay # along' a1
        r2 = aRay # at (1,0) # along' (180 - a2)

height :: PiecewiseLinear p => p -> Int -> Segment
height p n = aSegment
             # at' (vertex p n)
             #! normalTo (asLine (side p n))

vertexAngle' :: PiecewiseLinear p => p -> Int -> Angle
vertexAngle' p j = side p j `angleBetween` side p (j-1)

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
--
-- > let c  = aCircle # rotate 90
-- >     s1 = group $ modularScale 12 c
-- >     t  = aTriangle # scale 2
-- >     s2 = group $ modularScale 9 t
-- > in (c <+> s1) `beside` space 1 `beside` (s2 <+> t)
--
-- << figs/modularScale.svg>>
modularScale :: (Trans c, Curve c) => Int -> c -> [Decorated Point]
modularScale n = linearScale lf rng
  where n' = fromIntegral n
        rng = (/n') <$> [0..n'-1]
        lf = (`mod` n) . negate . round . (* n')

------------------------------------------------------------

instance WithOptions Point where
  defaultOptions p = mkOptions
    [ LabelPosition $ cmp p
    , LabelOffset (0 :+ 1)
    , LabelCorner (0, 0)
    , LabelAngle 0
    , Stroke "#444"
    , Fill "red"
    , Thickness "1" ]

instance WithOptions Label where
  defaultOptions p = mkOptions
    [ LabelPosition $ cmp p
    , LabelOffset 0
    , LabelCorner (0, 0)
    , LabelAngle 0 ]

instance WithOptions Circle where
  defaultOptions c = mkOptions
    [ LabelPosition $ c @-> 0
    , LabelOffset $ cmp $ normal c 0
    , LabelCorner (-1,0)
    , LabelAngle 0
    , Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

instance WithOptions Line where
  defaultOptions l = mkOptions
    [ LabelPosition $ l @-> 0.5
    , LabelOffset $ cmp $ normal l 0
    , Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

deriving via Line instance WithOptions Ray
deriving via Line instance WithOptions Segment

instance WithOptions Polyline where
  defaultOptions _ = mkOptions
    [ Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

deriving via Polyline instance WithOptions Polygon
deriving via Polyline instance WithOptions Triangle
deriving via Polyline instance WithOptions Rectangle

instance WithOptions Angle where
  defaultOptions an = mkOptions
    [ Stroke "white"
    , Fill "none"
    , Thickness "1.25"
    , MultiStroke 1
    , LabelPosition $ refPoint an + scale 30 (cmp (bisectrisse an)) ]

