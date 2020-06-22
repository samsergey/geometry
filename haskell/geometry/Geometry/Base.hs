{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FunctionalDependencies #-}
module Geometry.Base
  ( -- * Types
    -- ** Point represenations
    Cmp
  , XY
  -- ** Directed values
  , Direction (..)
  -- *** Direction isomorphisms
  , deg, asDeg
  , rad, asRad
  , turns, asTurns
  -- * Points in affine space
  , Affine (..), roundUp
  -- ** Predicates
  , isOrthogonal, isCollinear, isOpposite, isZero
  -- ** Vector and point operations
  , dot, det, cross, norm, distance, angle, normalize, columns, azimuth
  -- * Linear transformations
  , TMatrix, transformOrientation
  , Trans (..)
  , transformCmp, transformXY, transformAt
  -- ** transformations
  , translate', translate, superpose, at, at'
  , rotate, rotateAt', reflect, reflectAt
  , along, along', on
  , scale, scaleX, scaleY, scaleAt', scaleAt
  , scaleXAt', scaleXAt, scaleYAt', scaleYAt, scaleFig
  -- * Manifolds and curves
  , Manifold (..)
  , (->@), (->@?), (@->), (@->?)
  , start, paramL, projectL, distanceTo
  , Curve (..), PointLocation (..), ClosedCurve(..)
  -- * Figures
  , Figure (..), Box, pointBox
  -- ** Figures' size and bounding box corners.
  , figureHeight, figureWidth
  , corner, left, right, lower, upper
  -- * Fuzzy equality
  , AlmostEq
  -- ** Fuzzy inequalities
  , (~<=), (~>=), (~==)
  , (#)
  )
where

import Data.Fixed (mod')
import Data.Complex
import Data.List
import Data.List.Extra (minimumOn)
import Control.Applicative
import Control.Monad
import Data.Semigroup
import Data.Monoid
import Data.Maybe
import Data.Bool

------------------------------------------------------------
-- | Type alias for a complex number.
type Cmp = Complex Double

-- | Type alias for a coordinate representation.
type XY = (Double, Double)

------------------------------------------------------------

-- | Flipped application used for right chaining of transformations.
infixl 5 #
(#) = flip ($)

both f (a,b) = (f a, f b)

------------------------------------------------------------

infix 4 ~==

-- | Type class for values for which equality could be stated with some known tolerance.
class AlmostEq a where
  -- | The equality operator.
  (~==) :: a -> a -> Bool

instance AlmostEq Bool where  a ~== b = a == b
instance AlmostEq Int where a ~== b = a == b
instance AlmostEq Integer where  a ~== b = a == b

instance AlmostEq Double where
  a ~== b = abs (a - b) < 1e-8 || abs (a-b) < 1e-8 * abs(a+b)

instance (RealFloat a, Ord a, Fractional a, Num a, AlmostEq a) =>
         AlmostEq (Complex a) where
  a ~== b = magnitude (a - b) < 1e-8 || magnitude (a-b) < 1e-8 * magnitude(a+b)

instance (AlmostEq a, AlmostEq b) => AlmostEq (a, b) where
  (a1,b1) ~== (a2,b2) = a1 ~== a2 && b1 ~== b2

instance (AlmostEq a, AlmostEq b, AlmostEq c) => AlmostEq (a, b, c) where
  (a1,b1,c1) ~== (a2,b2,c2) = a1 ~== a2 && b1 ~== b2 && c1 ~== c2

instance (AlmostEq a) => AlmostEq [a] where
  as ~== bs = and $ zipWith (~==) as bs

instance AlmostEq a => AlmostEq (Maybe a) where
  Just a ~== Just b = a ~== b
  _ ~== _ = False
  
infix 4 ~<=
-- | The less or almost equal relation.
(~<=) :: (AlmostEq a, Ord a) => a -> a -> Bool
a ~<= b = a ~== b || a < b

infix 4 ~>=
-- | The greater or almost equal relation.
(~>=) :: (AlmostEq a, Ord a) => a -> a -> Bool
a ~>= b = a ~== b || a > b

------------------------------------------------------------

{- | The representation of a directed value isomorphic either to an angle
 or to a complex number or coordinates.
-}
newtype Direction = Direction Double
  deriving (Num, Fractional, Floating, Real, Enum)

-- | Constructs a directed value from an angle given in degrees.
asDeg :: Double -> Direction
asDeg = Direction . (`mod'` 360)

-- | Returns a representation of a directed value as an angle in degrees.
deg :: Direction -> Double
deg (Direction a) = a `mod'` 360

-- | Constructs a directed value from an angle given in radians.
asRad :: Double -> Direction
asRad r = asDeg (180 * r / pi)

-- | Returns a representation of a directed value as an angle in radians.
rad :: Direction -> Double
rad (Direction a) = (a / 180 * pi) `mod'` (2*pi)

-- | Constructs a directed value from a number of turns.
asTurns :: Double -> Direction
asTurns a = asDeg $ a*360

-- | Returns a representation of a directed value as a number of turns.
turns :: Direction -> Double
turns (Direction a) = (a / 360)  `mod'` 1

instance Show Direction where
  show a = show (round (deg a)) <> "°"

instance AlmostEq Direction where  a ~== b = rad a ~== rad b

instance Eq Direction  where  a == b = a ~== b

instance Ord Direction where  a <= b = a == b || rad a < rad b

------------------------------------------------------------

-- | The representation of a linear transformation matrix.
type TMatrix = ((Double, Double, Double),(Double, Double, Double))

-- | Class for objects which could be transformed linearly.
class Trans a where
  {-# MINIMAL transform #-}
  -- | The general linear transformation.
  transform :: TMatrix -> a -> a

instance Trans Cmp where
  transform = transformCmp

instance Trans XY where
  transform  = transformXY

instance Trans Direction where
  transform t  = asCmp . transformCmp t . cmp

instance Trans a => Trans (Maybe a) where
  transform t  = fmap (transform t)


-- | Returns 1 if transformation preserves curve orientation, and -1 otherwise.
transformOrientation :: TMatrix -> Double
transformOrientation ((a,b,_),(c,d,_)) = signum $ det ((a,b),(c,d))

-- | The rotation matrix.
rotateT :: Double -> TMatrix
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

-- | The reflection matrix.
reflectT :: Double -> TMatrix
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

-- | The translation matrix.
translateT :: Cmp -> TMatrix
translateT (dx :+ dy) = ((1, 0, dx), (0, 1, dy))

-- | The scaling matrix.
scaleT :: Double -> Double -> TMatrix
scaleT x y = ((x, 0, 0), (0, y, 0))

-- | The transformation of a complex number. Used in class implementations.
transformCmp :: TMatrix -> Cmp -> Cmp
transformCmp t = cmp . transformXY t . xy

-- | The transformation of coordinates. Used in class implementations.
transformXY :: TMatrix -> XY -> XY
transformXY ((a11, a12, sx), (a21, a22, sy)) (x, y) =
    (a12*y + a11*x + sx, a22*y + a21*x + sy)

-- | The transformation with shifted origin.
transformAt :: (Trans a, Affine p) => p -> (a -> a) -> a -> a
transformAt p t = translate' c . t . translate' (-c)
  where c = cmp p

-- | The translation of an object.
translate' :: (Trans a, Affine p) => p -> a -> a
translate' = transform . translateT . cmp

-- | The translation leading to a superposition of tho points.
superpose :: (Trans a, Affine p1, Affine p2) => p1 -> p2 -> a -> a
superpose p1 p2 = translate' (cmp p2 - cmp p1)

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
scaleAt' :: (Trans a, Affine p) => p -> Double -> a -> a
scaleAt' p s = transformAt p (scale s)

-- | The isotropic scaling of an object in x-direction against a given point.
scaleXAt' :: (Trans a, Affine p) => p -> Double -> a -> a
scaleXAt' p s = transformAt p (scaleX s)

-- | The isotropic scaling of an object in y-direction against a given point.
scaleYAt' :: (Trans a, Affine p) => p -> Double -> a -> a
scaleYAt' p s = transformAt p (scaleY s)

-- | The rotation of an object against the origin.
rotate :: Trans a => Direction -> a -> a
rotate = transform . rotateT . rad

-- | The rotation of an object against a given point.
rotateAt' :: (Trans a, Affine p) => p -> Direction -> a -> a
rotateAt' p a = transformAt p (rotate a)

-- | The reflection of an object against the direction passing through the origin.
reflect :: Trans a => Direction -> a -> a
reflect d = transform $ reflectT $ rad d

--reflectAt :: (Linear l, Trans a) => l -> a -> a
reflectAt l = transformAt (start l) (reflect (angle l))

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

{- | Rotates the figure which is `Affine` instance against it's `refPoint` so that it's
 refference angle (given by `angle`) councides with a given one.

> let a = aSegment # at (1,0) # along 30 #: "a"
>     t = aTriangle # along' a
>     s = aSquare # at (2,0) # along' t
> in a <+> t <+> s

<< figs/along.svg >>
-}
along :: (Figure f, Affine f) => Double -> (f -> f)
along d = along' (asDeg d)

-- | Locates an affine object on a given curve at
-- given parameter and aligns it along a tangent to a curve at this point.
--
-- > let c = aCircle
-- > in c <+>
-- >    aPoint # on c 0.1 <+>
-- >    aSegment # scale 0.5 # on c 0.3 <+>
-- >    aSquare # scale 0.5 # on c 0.6 <+>
-- >    aTriangle # scale 0.5 # on c 0.9
--
-- << figs/on.svg>>
--
on :: (Figure f, Affine f, Curve a c) => c -> Double -> (f -> f)
on c x = along' (tangent c x) . at' (c @-> x)

------------------------------------------------------------
{- | Class representing points in 2d affine space.
 Instances may be points, coordinates, complex numbers, 2d-vectors.
-}
class Affine a where
  {-# MINIMAL (asCmp, cmp) | (asXY, xy) #-}

  -- | Constructs an affine point from a complex number.
  asCmp :: Cmp -> a
  asCmp = asXY . xy

  -- | Returns a representation of an afine point as a complex number.
  cmp :: a -> Cmp
  cmp p = let (x, y) = xy p in x :+ y

  -- | Constructs an affine point from a coordinates.
  asXY :: XY -> a
  asXY = asCmp . cmp

  -- | Returns a representation of an afine point as a coordinate.
  xy :: a -> XY
  xy p = let x :+ y = cmp p in (x, y)

  -- | Returns x-coordinate of an afine point.
  getX :: a -> Double
  getX = fst . xy

  -- | Returns y-coordinate of an afine point.
  getY :: a -> Double
  getY = snd . xy
    
-- | Returns `True` if two points represent orthogonal vectors.
isOrthogonal :: (Affine a, Affine b) => a -> b -> Bool
isOrthogonal a b = not (isZero a || isZero b) && cmp a `dot` cmp b ~== 0

-- | Returns `True` if two points represent collinear vectors.
isCollinear :: (Affine a, Affine b) => a -> b -> Bool
isCollinear a b = not (isZero a || isZero b) && cmp a `cross` cmp b ~== 0

-- | Returns `True` if two points represent collinear opposite vectors.
isOpposite :: (Affine a, Affine b) => a -> b -> Bool
isOpposite a b = not (isZero a || isZero b) && cmp a + cmp b ~== 0

-- | Returns `True` if the vector is trivial or a point is equal to the origin.
isZero :: Affine a => a -> Bool
isZero a = cmp a ~== 0

-- | Returns the deriection given by two points.
azimuth :: (Affine a, Affine b) => a -> b -> Direction
azimuth p1 p2 = asCmp (cmp p2 - cmp p1)

-- | The dot product of two vectors (points).
dot :: (Affine a, Affine b) => a -> b -> Double
dot a b = let (xa, ya) = xy a
              (xb, yb) = xy b
              in xa*xb + ya*yb

-- | Determinant of a matrix, composed of two vector rows.
det :: (Affine a, Affine b) => (a, b) -> Double
det (a, b) = let (xa, ya) = xy a
                 (xb, yb) = xy b
             in xa*yb - ya*xb

-- | Z-component of a cross product of two vectors.
cross :: (Affine a, Affine b) => a -> b -> Double
cross a b = det (a, b)

-- | The norm of a vector, or a distance from a point to the origin.
norm :: Affine a => a -> Double
norm = magnitude . cmp

-- | The distance between tho points,
-- or a norm of the difference between two vectors.
distance :: (Affine a, Affine b) => a -> b -> Double
distance a b = magnitude (cmp a - cmp b)

-- | The normalized vector, or a projection of a point on a unit circle.
normalize :: Affine a => a -> a
normalize v
  | cmp v == 0 = v
  | otherwise = asCmp $ (1/norm v :+ 0) * cmp v

roundUp :: Affine a => Double -> a -> a
roundUp d = asXY . (\(x,y) -> (rounding x, rounding y)) . xy
  where rounding x = fromIntegral (ceiling (x /d)) * d

-- | The direction of the vector or a point.
angle :: Affine a => a -> Direction
angle = asCmp . cmp

-- | The matrix, composed of two vector columns.
columns :: Affine a => (a, a) -> (a, a)
columns (a, b) = ( asXY (getX a, getX b)
                 , asXY (getY a, getY b))

instance Affine Cmp where
  cmp = id
  asCmp = id

instance Affine XY where
  xy = id
  asXY = id

instance Affine Direction where
  cmp a = mkPolar 1 (rad a)
  asCmp = asRad . phase

instance Affine a => Affine (Maybe a) where
  cmp = maybe 0 cmp
  asCmp = pure . asCmp
------------------------------------------------------------

-- | Class representing 1-dimensional manifolds, e.g. lines, curves of angles, parameterized by real value.
class AlmostEq a => Manifold a m | m -> a where
  {-# MINIMAL param, project, (isContaining | (paramMaybe, projectMaybe)) #-}

  -- | Returns a point on a manifold for a given parameter.
  param :: m -> Double -> a

  -- | Returns a parameter, corresponding to a normal point projection on a manifold.
  project :: m -> a -> Double

  -- | Returns `True` if point belongs to the manifold.
  isContaining :: m -> a -> Bool
  isContaining c p = case projectMaybe c p >>= paramMaybe c  of
                       Just p' -> p' ~== p
                       Nothing -> False

  -- | Returns a point on a manifold for given parameter, or Nothing
  -- if parameter runs out of the range defined for a manifold.
  paramMaybe :: m -> Double -> Maybe a
  paramMaybe m x = if m `isContaining` p then Just p else Nothing
    where p = param m x

  -- | Returns a parameter for a point on a manifold, or nothing
  -- if parameter could not be found.
  projectMaybe :: m -> a -> Maybe Double
  projectMaybe m p = if inBounds x then Just x else Nothing
    where x = project m p
          inBounds x = case bounds m of
            [] -> True
            [a] -> x >= a
            [a, b] -> x >= a && x <= b

  -- | Returns bounds for a parameter of a manifold.
  -- [a, b] for finite, [a] for semibound manifolds (a is lower bound), [] for unbound manifolds
  bounds :: m -> [Double]
  bounds _ = [0, 1]
  
  -- | The internal length unit of the curve,
  --  which maps the parameter to the length of the curve.
  unit :: m -> Double
  unit _ = 1

instance (Affine a,  Manifold a m) => Manifold a (Maybe m) where
  param m' x = case m' of
                Just m -> param m x
                Nothing -> asCmp 0
  project m' p = case m' of
                   Just m -> project m p
                   Nothing -> 0

  isContaining m' p = case m' of
                        Just m -> isContaining m p
                        Nothing -> False

infix 8 @->
-- | Operator form of the `param` function.
(@->) ::  Manifold a m => m -> Double -> a
(@->) = param

infix 8 @->?
-- | Operator form of the `paramMaybe` function.
(@->?) :: Manifold a m => m -> Double -> Maybe a
(@->?) = paramMaybe

infix 8 ->@
-- | Operator form for `project` with flipped arguments:
--
-- prop>  p ->@ c  ==  project c p
--  
(->@) :: (Manifold a m) => a -> m -> Double
(->@) = flip project

infix 8 ->@?
-- | Operator for `projectMaybe` with flipped arguments:
--
-- prop>  p ->@? c  ==  projectMaybe c p
-- 
(->@?) :: (Affine a, Manifold a m) => a -> m -> Maybe Double
(->@?) = flip projectMaybe

-- | Point on the curve, parameterized by length.
paramL :: Manifold a m => m -> Double -> a
paramL m l = param m (l / unit m)

-- | Projection on a manifold, parameterized by length.
projectL :: (Manifold a m) => m -> a -> Double
projectL c p = unit c * project c p

-- | The distance between a manifold and a given point.
distanceTo :: (Affine a, Manifold a m) => a -> m -> Double
distanceTo p m = p `distance` p'
  where p' = case p ->@? m of
               Just x -> m @-> x
               Nothing -> minimumOn (distance p) (param m <$> bounds m)

-- | The starting point on the manifold. 
start :: Manifold a m => m -> a
start m = param m 0

------------------------------------------------------------

-- | Class representing a curve as a  continuous locally smooth manifold in affine space.
class (Figure c, Trans c, Affine a, Manifold a c) => Curve a c | c -> a  where
  {-# MINIMAL normal | tangent #-}

  -- | The tangent direction for a given parameter on the curve.
  tangent :: c -> Double -> Direction
  tangent c t = normal c t - 90

  -- | The normal direction for a given parameter on the curve.
  normal :: c -> Double -> Direction
  normal c t = tangent c t + 90

instance Curve a c => Curve a (Maybe c) where
  tangent c t = case c of
                  Just c -> tangent c t
                  Nothing -> 0
  normal c t = case c of
                  Just c -> normal c t
                  Nothing -> 0
                  
-- |  Class representing a closed region
class Curve a c => ClosedCurve a c | c -> a where
  {-# MINIMAL location | isEnclosing #-}
  
  -- | Returns the location of a point with respect to the region.
  location :: c -> a -> PointLocation
  location c p | isContaining c p = OnCurve
               | isEnclosing c p = Inside
               | otherwise = Outside

  -- | Returns `True` if point belongs to the region.
  isEnclosing :: c -> a -> Bool
  isEnclosing c p = location c p == Inside

instance ClosedCurve a c => ClosedCurve a (Maybe c) where
  location c p = case c of
                   Just c -> location c p
                   Nothing -> Outside
  isEnclosing c p = case c of
                   Just c -> isEnclosing c p
                   Nothing -> False
  
-- | The type representing the relation 'belongs to' between a point and a curve.
data PointLocation = Inside | Outside | OnCurve deriving (Show, Eq)

------------------------------------------------------------

type Box = ((Min Double, Min Double), (Max Double, Max Double))

pointBox :: Affine p => p -> Box
pointBox p = ((Min x, Min y), (Max x, Max y))
    where (x, y) = xy p

           
-- | Class representing the interface for a figure on a chart
class (Trans a) => Figure a where
  {-# MINIMAL isTrivial, box #-}

  -- | A rectangular bounding box enclosing a figure.
  box :: a -> Box

  -- | Returns `True` is figure is trivial in a certain sence.
  isTrivial :: a -> Bool

  -- | Returns `True` is figure is not trivial in a certain sence.
  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

  -- | Returns a refference point using for superposing and figure locataion.
  refPoint :: a -> Cmp
  refPoint = left . lower . corner

instance Bounded Double where
  minBound = -1/0
  maxBound = 1/0

instance Figure f => Figure (Maybe f) where
  box = maybe mempty box
  isTrivial  = maybe False isTrivial

figureWidth :: Figure f => f -> Double
figureWidth f = let ((Min xmin, Min ymin), (Max xmax, Max ymax)) = box f
          in abs $ xmax - xmin

figureHeight :: Figure f => f -> Double
figureHeight f = let ((Min xmin, Min ymin), (Max xmax, Max ymax)) = box f
           in abs $ ymax - ymin

corner p = ((x1:+y2, x2:+y2),(x1:+y1, x2:+y1))
    where ((Min x1,Min y1),(Max x2, Max y2)) = box p

lower = snd
upper = fst
right = snd
left  = fst

------------------------------------------------------------
