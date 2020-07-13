{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language ConstrainedClassMethods #-}

module Geometry.Figures
  ( origin
  , aPoint, aLabel
  , point, point'
  , pointOn, projectOn, intersectionPoints, closestTo
  , aligned
  , aLine, aRay, aSegment, oX, oY
  , line, line', ray, ray', segment, segment'
  , extendToLength, extendTo, normalSegment, heightFrom, clipBy
  , anAngle
  , angleBetween, angleWithin, innerAngle, bisectrisse
  , supplementary, vertical, reflex
  , aTriangle, triangle2a, triangle3s, aRightTriangle
  , aSquare, aRectangle, space
  , parametricPoly, polarPoly, regularPoly
  , aCircle, circle, circle'
  , linearScale, modularScale
  , translate, scaleAt, scaleXAt, scaleYAt, scaleFig
  , at', at, along', along, through', through
  , on, normalTo, flipAt
  , vertexAngle, altitude, median
 ) where

import Data.Complex
import Data.Maybe
import Data.List.Extra (minimumOn, sortOn)
import Control.Applicative
import Data.Fixed (mod')

import Geometry.Base
import Geometry.Point
import Geometry.Circle
import Geometry.Line
import Geometry.Angle
import Geometry.Polygon
import Geometry.Plot
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

{- | The constructor for a point with given coordinates.

>>> point (1,2)
<Point (1.0 2.0)>
-}
point :: XY -> Point
point = point'

{- | The generalized version of `point`.

> point' 0 #: "O" <+>
> point' (45 :: Direction) #: "A" <+>
> point' (1 :: Cmp) #: "B"
<< figs/points.svg >>
-}
point' :: Affine a => a -> Point
point' p = mkPoint (cmp p)

{- | Returns the point on a given curve.

> let c = aCircle
> in c <+>
>    pointOn c 0 #: "A" <+>
>    pointOn c 0.25 #: "B" <+>
>    pointOn c 0.667 #: "C"
<< figs/pointOn.svg >>
-}
pointOn :: Curve c => c -> Double -> Decorated Point
pointOn c t = mkPoint (c @-> t) #: loffs (cmp (normal c t))

{- | Returns a normal projection of the given point on the curve.

> let c = Plot (\t -> (t, sin t)) (0,6) # asPolyline
>     pA = point (1,0) #: "A"
>     pB = point (2,0) #: "B"
>     pC = point (4,0) #: "C"
>     pD = point (6,1) #: "D"
> in c <+> (pA <+> pA # projectOn c #: "A'" <+>
>           pB <+> pB # projectOn c #: "B'" <+>
>           pB <+> pC # projectOn c #: "C'" <+>
>           pD <+> pD # projectOn c #: "D'")
<< figs/projectOn.svg >>
-}
projectOn :: (APoint p, Curve c, Affine p) => c -> p -> Maybe (Decorated Point)
projectOn c p = pointOn c <$> (p ->@? c)

{- | Returns a list of intersection points as `Point` objects.

> let p1 = regularPoly 7
>     c = aCircle
> in p1 <+> c <+> group (intersectionPoints c p1)
<< figs/intersectionPoints.svg >>
-}
intersectionPoints :: ( Curve a, Curve b, Intersections a b )
                   => a -> b -> [Point]
intersectionPoints c1 c2 = point' <$> intersections c1 c2

-- | Returns a point from a list which is closest to a given one.
closestTo :: (Metric a, Affine a, Metric b, Affine b) => a -> [b] -> Maybe b
closestTo p [] = Nothing
closestTo p ps  = Just $ minimumOn (distance p) ps

{- | Returns @True@ if all points in a given list belong to one line
-}
aligned :: (AlmostEq a, Metric a, Affine a) => [a] -> Bool
aligned [] = False
aligned [_] = True
aligned [_, _] = True
aligned [x, y, z] | x ~= y = aligned [x, z]
                  | x ~= z || y ~= z = aligned [x, y]
                  | otherwise = line' x y `isContaining` z
aligned (x : y : ps) = (\p -> aligned [x, y, p]) `all` ps

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

{- | Returns a line, ray or a segment with given unit, in case of a segment -- with given length.

> group [aSegment # rotate x # extendToLength r
>       | x <- [0,5..360] 
>       , let r = 2 + sin (7 * rad x) ]
<< figs/extendToLength.svg >>
-}
extendToLength :: Double -> Segment -> Segment
extendToLength l s = s # through' (paramL l (asLine s))

{- | Returns a segment extended to a closest intersection point with a given curve.

> let t = aTriangle
>     s1 = aSegment # at (1,1)
>     s2 = aSegment # at (0.3,0.3)
> in t <+>
>    group [s1 # along a # extendTo t | a <- [0,10..360] ] <+>
>    group [s2 # along a # extendTo t | a <- [0,10..360] ]
<< figs/extendTo.svg >>
-}
extendTo :: (Curve c, Intersections Ray c)
         => c -> Segment -> Maybe Segment
extendTo c s = extend <$> closestTo (start s) (intersections (asRay s) c)
  where extend p = s # through' p

{- | Returns a segment normal to a given curve starting at given point.

> let t = aTriangle
>     pA = point (1,1) #: "A"
>     sa = t # heightFrom pA #: "a"
>     pB = point (1.2,0) #: "B"
>     sb = sa # heightFrom pB #: "b"
> in t <+> pA <+> sa <+> pB <+> sb
<< figs/heightFrom.svg >>
-}
heightFrom :: (Affine p, Curve c, Intersections Ray c)
           => p -> c -> Maybe Segment
heightFrom p c = (aSegment # at' p # normalTo c) >>= extendTo c

{- | Returns a list of segments as a result of clipping the line by a closed curve.

> let star = polarPoly (\x -> 2 + cos (5*x)) [0,0.1..1] # closePolyline
>     rs =  [ aSegment # scale 3 # rotate 35 # at (-1, x)
>           | x <- [-4,-3.5..4] ]
> in star <+> foldMap (group . clipBy star) rs
g
<< figs/clipBy.svg >>
-}
clipBy :: (Linear l, Intersections l c, ClosedCurve c)
       => c -> l -> [Segment]
clipBy c l = filter internal $ Segment <$> zip ints (tail ints) 
  where
    ints = sortOn (->@ l) $ intersections l c <> ends
    internal s = c `isEnclosing` (s @-> 0.5)
    ends = cmp . (l @->) <$> case bounds l of
                               Unbound -> []
                               Semibound -> [0]
                               Bound -> [0,1]

-- | A generalized version of `through`.
through' :: (Metric p, Affine p, Linear l) => p -> (l -> l)
through' p l = l
               # along' (azimuth p0 p)
               # scaleAt' p0 (distance p0 p / unit l)
  where p0 = start l

{- | Turns and extends the line so that it passes through a given point.

> let pA = point (2,3) #: "A"
>     pB = point (3,2) #: "B"
> in aSegment # through' pA <+>
>    aRay # through (3,2) <+>
>    pA <+> pB <+> origin
<< figs/through.svg>>
-}
through :: Linear l => XY -> (l -> l)
through = through'

{- | If possible, turns the line so that it becomes normal to a given curve, pointing towards a curve.

> let c = Plot $ (\t -> t :+ sin t) . (*6)
> in c <+>
>    group [ aSegment # at (x,0) # normalTo c
>          | x <- [0,1..7] ]
<< figs/normalTo.svg >>
-}
normalTo :: (Curve c, Linear l) => c -> l -> Maybe l
normalTo c l = turn <*> Just l
  where s = start l
        turn = if c `isContaining` s
               then along' . normal c <$> (s ->@? c)
               else along' . ray' s <$> (point' s # projectOn c)

{- | If possible, returns a line segment normal to the curve starting at a given parameter.

> let t = aTriangle
> in t
>    <+> t # normalSegment 0.2 #: "1"
>    <+> t # normalSegment 0.5 #: "2"
>    <+> t # normalSegment (2/3) #: "3"
>    <+> t # normalSegment 1 #: "4"
<< figs/normalSegment.svg >>
-}
normalSegment :: Curve c => Double -> c -> Maybe Segment
normalSegment x c =
  do p <- paramMaybe c x
     aSegment # at' p # normalTo c

{- | Reflects the curve  at a given parameter against the normal, if it exists, or does nothing otherwise.
-}
flipAt :: Curve c => Double -> c -> c
flipAt x c = case c # normalSegment x of
               Just n -> c # reflectAt n
               Nothing -> c

------------------------------------------------------------

{- $ang| The `Angle` mark looks like a small labeled arc and two segments.

>>> anAngle 30
<Angle 30 (0.0, 0.0)>

> writeSVG 400 "figs/angle1.svg" $
>   let t = triangle2a 30 60
>      a1 = anAngle 30 #: "#" <> loffs ((-1):+1)
>      a2 = anAngle 90 # on (side t 2) 0 #: "#"
>      a3 = vertexAngle t 1 #: "#"
>  in t <+> a1 <+> a2 <+> a3

<<figs/angle1.svg>>

Angle is a `Manifold Direction` instance:

>>> Angle 0 0 90 @-> 0.5
45°
>>>  Angle 0 0 90 @-> 0
0°
>>> Angle 0 0 90 @-> 1
90°

>>> 45 ->@ Angle 0 0 90
0.5
>>> 180 ->@ Angle 0 0 90
2.0
>>> 0 ->@ Angle 0 10 30
17.5$ang
-}

-- | The template for an angle with given value.
anAngle :: Direction -> Angle
anAngle = Angle 0 0

-- | Returns the angle equal to the angle between two lines, located on the first one.
angleBetween :: (Linear l1,  Linear l2) => l1 -> l2 -> Angle
angleBetween l1 l2 = anAngle (angle l2 - angle l1)
                     # at' (start l1)
                     # along' l1

-- | Returns the Angle mark for the angle, formed by three points.
angleWithin :: (Affine a1, Affine a2, Affine a3)
            => a1 -> a2 -> a3 -> Angle
angleWithin p1 p2 p3 = angleBetween (ray' p2 p1) (ray' p2 p3)

{- | Returns the Angle mark for the angle at given polyline vertex.

<< figs/vertexAngle.svg >>
-}
vertexAngle :: PiecewiseLinear p => Int -> p -> Angle
vertexAngle n p = angleWithin p3 p2 p1 # innerAngle
  where p1 = p # vertex (n-1)
        p2 = p # vertex n
        p3 = p # vertex (n+1)

{- | Returns a ray, representng the bisectrisse of a given angle.

> let t = triangle2a 50 70
>     r1 = t # vertexAngle 0 # bisectrisse #: thin <> white
>     r2 = t # vertexAngle 1 # bisectrisse #: thin <> white
>     r3 = t # vertexAngle 2 # bisectrisse #: thin <> white
>     p = head $ intersectionPoints r1 r2
>     c = circle' (p `distanceTo` side 0 t) p
> in t <+> r1 <+> r2 <+> r3 <+> p <+> c

<< figs/bisectrisse.svg >>
-}
bisectrisse :: Angular a => a -> Ray
bisectrisse an = aRay # at' (refPoint an) # along' (an @-> 0.5)

{- | The supplementary angle.

> let a = anAngle 60 #: "a"
>     b = a # supplementary #: "b"
> in aRay <+> aRay # rotate 60 <+> a <+> b
<<figs/angle2.svg>>
-} 
supplementary :: Angular an => an -> an
supplementary = asAngle . (\(Angle p s e) -> Angle p e (s + 180)) . toAngle

{- | The vertical angle.

> let a = anAngle 60 #: "a"
>     b = a # vertical #: "b"
> in aRay <+> aRay # rotate 60 <+> a <+> b
<<figs/angle3.svg>>
-} 
vertical :: Angular an => an -> an
vertical = rotate 180

{- | The reflex angle.

> let a = anAngle 60 #: "a"
>     b = a # reflex #: "b"
> in aRay <+> aRay # rotate 60 <+> a <+> b
<<figs/angle4.svg>>
-} 
reflex :: Angular an => an -> an
reflex an = asAngle $ Angle (refPoint an) e s
  where s = angleStart an
        e = angleEnd an


-- | Returns the inner angle (less than `pi`) for a given one.
innerAngle :: Angular an => an -> an
innerAngle an | deg (angleValue an) > 180 = reflex an
              | otherwise = an

------------------------------------------------------------

{- | Constructs a parametric graph as a `Polyline`.

<< figs/parametricPoly.svg >>
-}
parametricPoly :: (Double -> XY) -> [Double] -> Polyline
parametricPoly f range =
  mkPolyline [ x :+ y | t <- range , let (x,y) = f t ]

{- | Constructs a polar graph as a `Polyline`.

> polarPoly (\x -> 2 + cos (5*x)) [0,0.1..1] # closePolyline

<< figs/polarPoly.svg >>
-}
polarPoly :: (Double -> Double) -> [Double] -> Polyline
polarPoly rho range =
  mkPolyline [ mkPolar (rho phi) phi | x <- range
                                     , let phi = 2*pi*x ]

{- | Constructs a regular polygon with given number of sides, enscribed in a unit circle.

<< figs/regularPoly.svg >>
-}
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

-- | An invisible square with given size, used for spacing aligned objects. 
space :: Double -> Decorated Rectangle
space a = aSquare # scale a #: invisible

-- | The template for a Rectangle.
aRectangle :: Double -> Double -> Rectangle
aRectangle a b = aSquare # scaleX a . scaleY b

{- | Returns a triangle with base 1 and two given angles.

> -- a family of triangles, sharing one side and opposite vertex angle value (30°)
> group [ triangle2a a (150-a) #: thin | a <- [10,20..170] ]

<< figs/triangle2a.svg >>
-}
triangle2a :: Direction -> Direction -> Triangle
triangle2a a1 a2 = case intersections r1 r2 of
                    [p] -> mkTriangle (0,0) (1,0) (xy p)
                    [] -> mkTriangle @XY (0,0) (1,0) (0,0)
  where r1 = aRay # along' a1
        r2 = aRay # at (1,0) # along' (180 - a2)

{- | Returns a triangle with given three sides.

<< figs/triangle3s.svg >>
-}
triangle3s :: Double -> Double -> Double -> Maybe Triangle
triangle3s a b c = case intersections c1 c2 of
                     [] -> Nothing
                     p:_ -> Just $ Triangle [0, a :+ 0, p]
  where c1 = aCircle # scale b
        c2 = aCircle # scale c # at (a, 0)

{- | A isosceles right triangle with unit side.
<< figs/rightTriangle.svg >>
-}
aRightTriangle = RightTriangle $ triangle2a 90 45
  
------------------------------------------------------------
{- | The median of the polygon from given vertex to given side.
-}
median :: PiecewiseLinear p
  => Int -- ^ vertex index
  -> Int -- ^ side index
  -> p -> Segment
median v s t = Segment (vertex v t, side s t @-> 0.5)

{- | The altitude of the polygon from given vertex to given side.
-}
altitude :: PiecewiseLinear p
  => Int  -- ^ vertex index
  -> Int  -- ^ side index
  -> p -> Segment
altitude v s p = heightFrom (vertex v p) (asLine (side s p)) # fromJust 

------------------------------------------------------------

-- | Creates a scale as a list of labeled points on a given curve
linearScale :: (Show s, Curve c)
            => (Double -> s) -- ^ labeling function
            -> [Double] -- ^ range of the curve parameter
            -> c -- ^ the curve
            -> [Decorated Point]
linearScale fn rng c = [ pointOn c x #:: label (show (fn x))
                       | x <- rng ]

{- | Creates a circular integer scale, representing modular arithmetics.

> let c  = aCircle # rotate 90
>     s1 = group $ modularScale 12 c
>     t  = aTriangle # scale 2
>     s2 = group $ modularScale 9 t
> in (c <+> s1) `beside` space 1 `beside` (s2 <+> t)
<< figs/modularScale.svg>>
-}
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

instance Pnt a => WithOptions (Plot a) where
  defaultOptions = defaultOptions . asPolyline
  
instance Pnt a => WithOptions (ClosedPlot a) where
  defaultOptions = defaultOptions . asPolyline

--------------------------------------------------------------------------------
