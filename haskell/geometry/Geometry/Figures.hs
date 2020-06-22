{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}

module Geometry.Figures
  ( origin
  , aPoint, aLabel
  , point, point'
  , pointOn, projectOn, intersectionPoints, closestTo
  , aLine, aRay, aSegment, oX, oY
  , line, line', ray, ray', segment, segment'
  , extendToLength, extendTo, normalSegment, heightTo, clipBy
  , anAngle
  , angleBetween, angleWithin, bisectrisse
  , supplementary, vertical, reflex
  , aTriangle, triangle2a
  , aSquare, aRectangle, space
  , parametricPoly, polarPoly, regularPoly
  , aCircle, circle, circle'
  , linearScale, modularScale
  , translate, scaleAt, scaleXAt, scaleYAt, scaleFig
  , at', at, along', along, through', through
  , on, normalTo, flipAt
  , vertexAngle, height
 ) where

import Data.Complex
import Data.Maybe
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

-- | A point at the origin. Equivalent to `origin`.
aPoint :: Point
aPoint = asCmp 0

-- | A label: the invisible point which could be labeled using `#:` operator.
aLabel :: Label
aLabel = mkLabel origin

-- | The constructor for a point with given coordinates.
--
-- >>> point (1,2)
-- <Point (1.0 2.0)>
--
point :: XY -> Point
point = point'

-- | The generalized version of `point`.
--
-- > point' 0 #: "O" <+>
-- > point' (45 :: Direction) #: "A" <+>
-- > point' (1 :: CN) #: "B"
-- << figs/points.svg >>
--
point' :: Affine a => a -> Point
point' p = mkPoint (cmp p)

-- | The point on a given curve.
--
-- > let c = aCircle
-- > in c <+>
-- >    pointOn c 0 #: "A" <+>
-- >    pointOn c 0.25 #: "B" <+>
-- >    pointOn c 0.667 #: "C"
-- << figs/pointOn.svg >>
--
pointOn :: Curve a c => c -> Double -> Decorated Point
pointOn c t = mkPoint (c @-> t) #: loffs (cmp (normal c t))

-- | Returns a normal projection of the given point on the curve.
--
-- > let c = Plot (\t -> (t, sin t)) (0,6) # asPolyline
-- >     pA = point (1,0) #: "A"
-- >     pB = point (2,0) #: "B"
-- >     pC = point (4,0) #: "C"
-- >     pD = point (6,1) #: "D"
-- > in c <+> (pA <+> pA # projectOn c #: "A'" <+>
-- >           pB <+> pB # projectOn c #: "B'" <+>
-- >           pB <+> pC # projectOn c #: "C'" <+>
-- >           pD <+> pD # projectOn c #: "D'")
-- << figs/projectOn.svg >>
--
projectOn :: (APoint p, Curve CN c, Affine p) => c -> p -> Maybe (Decorated Point)
projectOn c p = pointOn c <$> (cmp p ->@? c)

-- | Returns a list of intersection points as `Point` objects.
--
-- > let p1 = regularPoly 7
-- >     c = aCircle
-- > in p1 <+> c <+> group (intersectionPoints c p1)
-- << figs/intersectionPoints.svg >>
--
intersectionPoints :: ( Curve CN a, Curve CN b, Intersections a b )
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
--
-- > group [aSegment # rotate x # extendToLength r
-- >       | x <- [0,5..360] 
-- >       , let r = 2 + sin (7 * rad x) ]
--
-- << figs/extendToLength.svg >>
--
extendToLength :: Double -> Segment -> Segment
extendToLength l s = s # through' (paramL (asLine s) l)

-- | Returns a segment extended to a closest intersection point with a given curve.
--
-- > let t = aTriangle
-- >     s1 = aSegment # at (1,1)
-- >     s2 = aSegment # at (0.3,0.3)
-- > in t <+>
-- >    group [s1 # along a # extendTo t | a <- [0,10..360] ] <+>
-- >    group [s2 # along a # extendTo t | a <- [0,10..360] ]
-- << figs/extendTo.svg >>
--
extendTo :: (Curve CN c, Intersections Ray c)
         => c -> Segment -> Maybe Segment
extendTo c s = extend <$> closestTo (start s) (intersections (asRay s) c)
  where extend p = s # through' p

-- | Returns a segment normal to a given curve starting at given point.
--
-- >>> point (1,1) # heightTo oX
-- Just (Segment (1.0 :+ 1.0, 1.0 :+ 0.0))
-- >>> point (-1,1) # heightTo aRay
-- Nothing
--
heightTo :: (Affine p, Curve CN c, Intersections Ray c)
         => c -> p -> Maybe Segment
heightTo c p = (aSegment # at' p # normalTo c) >>= extendTo c

-- | Returns a list of segments as a result of clipping the line
-- by a closed curve.
clipBy :: (Linear l, Intersections l c, Figure c, ClosedCurve CN c)
       => l -> c -> [Segment]
clipBy l c = filter internal $ Segment <$> zip ints (tail ints) 
  where
    ints = sortOn (project l) $ intersections l c <> ends
    internal s = c `isEnclosing` (s @-> 0.5)
    ends = (l @->) <$> bounds l

-- | A generalized version of `through`.
through' :: (Affine p, Linear l) => p -> (l -> l)
through' p l = l
               # along' (azimuth p0 p)
               # scaleAt' p0 (distance p0 p / unit l)
  where p0 = start l

-- | Turns and extends the line so that it passes through a given point.
--
-- > let pA = point (2,3) #: "A"
-- >     pB = point (3,2) #: "B"
-- > in aSegment # through' pA <+>
-- >    aRay # through (3,2) <+>
-- >    pA <+> pB <+> origin
--
-- << figs/through.svg>>
--
through :: Linear l => XY -> (l -> l)
through = through'

-- | If possible, turns the line so that it becomes normal to a given curve, pointing towards a curve.
--
-- > let c = Plot $ (\t -> t :+ sin t) . (*6)
-- > in c <+>
-- >    group [ aSegment # at (x,0) # normalTo c
-- >          | x <- [0,1..7] ]
--
-- << figs/normalTo.svg >>
--
normalTo :: (Curve CN c, Linear l) => c -> l -> Maybe l
normalTo c l = turn <*> Just l
  where s = start l
        turn = if c `isContaining` s
               then along' . normal c <$> (s ->@? c)
               else along' . ray' s <$> (s # projectOn c)

-- | Reflects the curve  at a given parameter against the normal, if it exists, or does nothing otherwise.
flipAt :: (Curve CN c) => Double -> c -> c
flipAt x c = case normalSegment c x of
               Just n -> c # reflectAt n
               Nothing -> c

-- | If possible, returns a line segment normal to the curve starting at a given parameter
normalSegment :: Curve CN c => c -> Double -> Maybe Segment
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
             # fromJust . normalTo (asLine (side p n))

vertexAngle' :: PiecewiseLinear p => p -> Int -> Angle
vertexAngle' p j = side p j `angleBetween` side p (j-1)

------------------------------------------------------------

-- | Creates a scale as a list of labeled points on a given curve
linearScale :: (Show s, Curve a c)
            => (Double -> s) -- ^ labeling function
            -> [Double] -- ^ range of the curve parameter
            -> c -- ^ the curve
            -> [Decorated Point]
linearScale fn rng c = [ pointOn c x #:: label (show (fn x))
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
modularScale :: (Trans c, Affine a, Curve a c) => Int -> c -> [Decorated Point]
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

