{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Base
  ( -- * Types
    CN
  , XY
  , Partial
  -- ** Directed values
  , Angular (..)
  -- ** Angular isomorphisms
  , deg, asDeg
  , rad, asRad
  , turns, asTurns
  -- ** Linear transformations
  , TMatrix
  , rotateT, reflectT, translateT, scaleT
  , transformOrientation
  -- * Classes
  -- ** Points in affine space
  , Affine (..)
  , roundUp
  -- *** Predicates
  , isOrthogonal, isCollinear, isOpposite, isZero
  -- *** Vector and point operations
  , dot, det, cross, norm, distance, angle, normalize, columns, azimuth
  -- ** Linear transformations
  , Trans (..)
  , transformCN, transformXY, transformAt
  -- *** transformations
  , translate', superpose
  , scale, scaleX, scaleY, scaleAt', scaleXAt', scaleYAt'
  , rotate, rotateAt', reflect, reflectAt
  -- ** Curves
  , Curve (..), PointLocation (..)
  , (->@), (@->), (->@!), (@->!)
  , paramL, projectL
  -- ** Intersections of curves
  , Intersections (..)
  -- ** Figures
  , Figure (..), Box, pointBox
  , height, width
  , corner, left, right, lower, upper
  -- ** Fuzzy equality
  , AlmostEq
  -- *** Fuzzy inequalities
  , (~<=), (~>=), (~==)
  -- * Misc
  , (#)
  )
where

import Data.Fixed (mod')
import Data.Complex
import Data.List
import Data.List.Extra (minimumOn)
import Control.Applicative
import Data.Semigroup
import Data.Monoid
import Data.Either

------------------------------------------------------------
-- | Type alias for a complex number.
type CN = Complex Double

-- | Type alias for a coordinate representation.
type XY = (Double, Double)

-- | Type alias for a partial result.
type Partial a = Either String a

------------------------------------------------------------

-- | Flipped application used for right chaining of transformations.
infixl 5 #
(#) = flip ($)

both f (a,b) = (f a, f b)

------------------------------------------------------------

infix 4 ~==

-- | Type class for values for which equality could be stated
-- with some known tolerance.
class AlmostEq a where
  -- | The equality operator.
  (~==) :: a -> a -> Bool

instance AlmostEq Int where a ~== b = a == b

instance AlmostEq Integer where  a ~== b = a == b

instance AlmostEq Double where
  a ~== b = abs (a - b) < 1e-10 || abs (a-b) < 1e-10 * abs(a+b)

instance (RealFloat a, Ord a, Fractional a, Num a, AlmostEq a) => AlmostEq (Complex a) where
  a ~== b = magnitude (a - b) < 1e-10 || magnitude (a-b) < 1e-10 * magnitude(a+b)

instance (AlmostEq a, AlmostEq b) => AlmostEq (a, b) where
  (a1,b1) ~== (a2,b2) = a1 ~== a2 && b1 ~== b2

instance (AlmostEq a) => AlmostEq [a] where
  as ~== bs = and $ zipWith (~==) as bs

infix 4 ~<=
-- | The less or almost equal relation.
(~<=) :: (AlmostEq a, Ord a) => a -> a -> Bool
a ~<= b = a ~== b || a < b

infix 4 ~>=
-- | The greater or almost equal relation.
(~>=) :: (AlmostEq a, Ord a) => a -> a -> Bool
a ~>= b = a ~== b || a > b

------------------------------------------------------------

-- | The representation of a directed value isomorphic either to an angle
-- or to a complex number or coordinates.
newtype Angular = Angular Double

-- | Constructs a directed value from an angle given in radians.
asRad :: Double -> Angular
asRad = Angular . (`mod'` (2*pi))

-- | Returns a representation of a directed value as an angle in radians.
rad :: Angular -> Double
rad (Angular a) = a  `mod'` (2*pi)

-- | Constructs a directed value from an angle given in degrees.
asDeg :: Double -> Angular
asDeg a = asRad $ a / 180*pi

-- | Returns a representation of a directed value as an angle in degrees.
deg :: Angular -> Double
deg (Angular a) = (a * 180 / pi) `mod'` 360

-- | Constructs a directed value from a number of turns.
asTurns :: Double -> Angular
asTurns a = asRad $ a*2*pi

-- | Returns a representation of a directed value as a number of turns.
turns :: Angular -> Double
turns (Angular a) = (a / (2*pi))  `mod'` 1

instance Show Angular where
  show a = show (round (deg a)) <> "°"

instance AlmostEq Angular where  a ~== b = rad a ~== rad b

instance Eq Angular  where  a == b = a ~== b

instance Ord Angular where  a <= b = a == b || rad a < rad b

instance Num Angular where
  fromInteger = asDeg . fromIntegral
  (+) = withAngular2 (+)
  (*) = withAngular2 (*)
  negate = withAngular negate
  abs = withAngular abs
  signum = withAngular signum

instance Fractional Angular where
  fromRational = Angular . fromRational
  recip (Angular r) = Angular (1 / r) 

instance Floating Angular where
  pi = Angular pi
  exp (Angular a) = Angular $ exp a
  log (Angular a) = Angular $ log a
  sin (Angular a) = Angular $ sin a
  cos (Angular a) = Angular $ cos a
  asin (Angular a) = Angular $ asin a
  acos (Angular a) = Angular $ acos a
  atan (Angular a) = Angular $ atan a
  sinh (Angular a) = Angular $ sinh a
  cosh (Angular a) = Angular $ cosh a
  asinh (Angular a) = Angular $ asinh a
  acosh (Angular a) = Angular $ acosh a
  atanh (Angular a) = Angular $ atanh a

instance Real Angular where
  toRational (Angular a) = toRational a


withAngular op a = asRad $ op (rad a)

withAngular2 op a b = asRad (rad a `op` rad b)

------------------------------------------------------------

-- | The representation of a linear transformation matrix.
type TMatrix = ((Double, Double, Double),(Double, Double, Double))

-- | Class for objects which could be transformed linearly.
class Trans a where
  {-# MINIMAL transform #-}
  -- | The general linear transformation.
  transform :: TMatrix -> a -> a

instance Trans CN where
  transform t = cmp . transformXY t . coord

instance Trans XY where
  transform  = transformXY

instance Trans Angular where
  transform t  = asCmp . cmp . transformXY t . coord

-- | Returns 1 if transformation preserves curve orientation, and -1 otherwise.
transformOrientation :: TMatrix -> Double
transformOrientation ((a,b,_),(c,d,_)) =
  signum $ det ((a,b),(c,d))

-- | The rotation matrix.
rotateT :: Double -> TMatrix
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

-- | The reflection matrix.
reflectT :: Double -> TMatrix
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

-- | The translation matrix.
translateT :: CN -> TMatrix
translateT (dx :+ dy) = ((1, 0, dx), (0, 1, dy))

-- | The scaling matrix.
scaleT :: Double -> Double -> TMatrix
scaleT x y = ((x, 0, 0), (0, y, 0))

-- | The transformation of a complex number. Used in class implementations.
transformCN :: TMatrix -> CN -> CN
transformCN t = cmp . transformXY t . coord

-- | The transformation of coordinates. Used in class implementations.
transformXY :: TMatrix -> XY -> XY
transformXY ((a11, a12, sx), (a21, a22, sy)) (x, y) =
    (a12*y + a11*x + sx, a22*y + a21*x + sy)

-- | The transformation with shifted origin.
transformAt :: (Trans a, Affine p) => p -> (a -> a) -> a -> a
transformAt p t = translate' xy . t . translate' (-xy)
  where xy = cmp p

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
rotate :: Trans a => Angular -> a -> a
rotate = transform . rotateT . rad

-- | The rotation of an object against a given point.
rotateAt' :: (Trans a, Affine p) => p -> Angular -> a -> a
rotateAt' p a = transformAt p (rotate a)

-- | The reflection of an object against the direction passing through the origin.
reflect :: Trans a => Angular -> a -> a
reflect d = transform $ reflectT $ rad d

--reflectAt :: (Linear l, Trans a) => l -> a -> a
reflectAt l = transformAt (start l) (reflect (angle l))

------------------------------------------------------------
-- | Class representing points in 2d affine space.
-- Instances may be points, coordinates, complex numbers, 2d-vectors.
class Affine a where
  {-# MINIMAL (asCmp, cmp) | (asCoord, coord) #-}

  -- | Constructs an affine point from a complex number.
  asCmp :: CN -> a
  asCmp = asCoord . coord

  -- | Returns a representation of an afine point as a complex number.
  cmp :: a -> CN
  cmp p = let (x, y) = coord p in x :+ y

  -- | Constructs an affine point from a coordinates.
  asCoord :: XY -> a
  asCoord = asCmp . cmp

  -- | Returns a representation of an afine point as a coordinate.
  coord :: a -> XY
  coord p = let x :+ y = cmp p in (x, y)

  -- | Returns x-coordinate of an afine point.
  getX :: a -> Double
  getX = fst . coord

  -- | Returns y-coordinate of an afine point.
  getY :: a -> Double
  getY = snd . coord
    
-- | Returns True if two points represent orthogonal vectors.
isOrthogonal :: (Affine a, Affine b) => a -> b -> Bool
isOrthogonal  a b = cmp a `dot` cmp b ~== 0

-- | Returns True if two points represent collinear vectors.
isCollinear :: (Affine a, Affine b) => a -> b -> Bool
isCollinear a b = cmp a `cross` cmp b ~== 0

-- | Returns True if two points represent collinear opposite vectors.
isOpposite :: (Affine a, Affine b) => a -> b -> Bool
isOpposite a b = cmp a + cmp b ~== 0

-- | Returns True if the vector is trivial or a point is equal to the origin.
isZero :: Affine a => a -> Bool
isZero a = cmp a ~== 0

-- | The ngle between tho two points
azimuth :: (Affine a, Affine b) => a -> b -> Angular
azimuth p1 p2 = asCmp (cmp p2 - cmp p1)

-- | The dot product of two points.
dot :: (Affine a, Affine b) => a -> b -> Double
dot a b = let (xa, ya) = coord a
              (xb, yb) = coord b
              in xa*xb + ya*yb

-- | Determinant of a matrix, composed of two vector rows.
-- Equivalent to `cross`.
det :: (Affine a, Affine b) => (a, b) -> Double
det (a, b) = let (xa, ya) = coord a
                 (xb, yb) = coord b
             in xa*yb - ya*xb

-- | Z-component of a cross product of two vectors.
-- Equivalent to `det`.
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
roundUp d = asCoord . (\(x,y) -> (rounding x, rounding y)) . coord
  where rounding x = fromIntegral (ceiling (x /d)) * d

-- | The direction of the vector or a point.
angle :: Affine a => a -> Angular
angle = asCmp . cmp

-- | The matrix, composed of two vector columns.
columns :: Affine a => (a, a) -> (a, a)
columns (a, b) = ( asCoord (getX a, getX b)
                 , asCoord (getY a, getY b))

instance Affine CN where
  cmp = id
  asCmp = id

instance Affine XY where
  coord = id
  asCoord = id

instance Affine Angular where
  cmp a = mkPolar 1 (rad a)
  asCmp = Angular . phase

------------------------------------------------------------

-- | The type representing the relation 'belongs to' between a point and a curve.
data PointLocation = Inside | Outside | OnCurve deriving (Show, Eq)

-- | Class representing a curve parameterized by a real number.
-- For finite curves parameter runs from 0 to 1, where 0 is a start
-- and 1 is the end of the curve. The length scale is given by `unit`
-- function.
--
-- All implementations must obay isomorphisms laws:
--
-- prop>  isFinite c          ==>  (project c . param c) x == x `mod'` 1
-- prop>  not isFinite c      ==>  (project c . param c) x == x
-- prop>  c `isContaining` p  ==>  (param c . project c) p == p
--
class Show c => Curve c where
  {-# MINIMAL param,
              project,
              (normal | tangent),
              distanceTo,
              (location | (isContaining, isEnclosing)) #-}

  -- | Returns a point on a curve for given parameter.
  param :: c -> Double -> Partial CN
  param c _ = Left $ "param is unplemented for " <> show c

  -- | Returns a parameter, corresponding to a normal point projection on a curve.
  project :: Affine p => c -> p -> Partial Double
  project c _ = Left $ "project is unplemented for " <> show c

  -- | The internal length unit of the curve,
  --  which maps the parameter to the length of the curve.
  unit :: c -> Double
  unit _ = 1

  -- | The distance between a curve and a point.
  distanceTo :: Affine p => p -> c -> Double

  -- | The tangent direction for a given parameter on the curve.
  tangent :: c -> Double -> Partial Angular
  tangent f t = (90 +) <$> normal f t

  -- | The normal direction for a given parameter on the curve.
  normal :: c -> Double -> Partial Angular
  normal f t = (90 +) <$> tangent f t

  -- | Is set `True` if the curve is closed.
  isClosed :: c -> Bool
  isClosed _ = False

  -- | Is set `True` if the curve is finite.
  isFinite :: c -> Bool
  isFinite _ = True
  
  -- | Returns the location of a point with respect to the curve.
  location :: Affine p => p -> c -> PointLocation
  location p c | isContaining c p = OnCurve
               | isClosed c && isEnclosing c p = Inside
               | otherwise = Outside

  -- | Returns `True` if point belongs to the curve.
  isContaining :: Affine p => c -> p -> Bool
  isContaining c p = location p c == OnCurve

  -- | Returns `True` if point belongs to the area bound by closed curve.
  isEnclosing :: Affine p => c -> p -> Bool
  isEnclosing c p = location p c == Inside

  -- | Returns th orientation of a curve.
  -- Orientation affects the direction of tangent and normal vectors.
  orientation :: c -> Double
  orientation _ = 1

  -- | The starting point on the curve. 
  start :: c -> CN
  start c = fromRight 0 (param c 0)

infix 8 @->
-- | Operator for `param`
(@->) ::  Curve c => c -> Double -> Partial CN
(@->) = param

infix 8 @->!
-- | Operator for `param`
(@->!) ::  Curve c => c -> Double -> CN
c @->! x = fromRight 0 $ param c x

infix 8 ->@
-- | Operator for `project` with flipped arguments:
--
-- prop>  p ->@ c  ==  project c p
--  
(->@) :: (Curve c, Affine p) => p -> c -> Partial Double
(->@) = flip project

infix 8 ->@!
(->@!) :: (Curve c, Affine p) => p -> c -> Double
p ->@! c = fromRight 0 $ project c p


-- | Point on the curve, parameterized by length.
paramL :: Curve c => c -> Double -> Partial CN
paramL c l = param c (l / unit c)

-- | Projection on a curve, parameterized by length.
projectL :: (Affine p, Curve c) => c -> p -> Partial Double
projectL c p = (unit c *) <$> project c p
   
------------------------------------------------------------
-- | Class provides `intersections` function returning a list (possible empty)
-- of intersection points (co-dimension 1).
class (Curve a, Curve b) => Intersections a b where
  intersections :: a -> b -> [CN]

-- | Returns `True` if tho curves have intersection points.
isIntersecting :: Intersections a b => a -> b -> Bool
isIntersecting a b = not . null $ intersections a b

------------------------------------------------------------

type Box = ((Min Double, Min Double), (Max Double, Max Double))

pointBox :: Affine p => p -> Box
pointBox p = ((Min x, Min y), (Max x, Max y))
    where (x, y) = coord p

           
-- | Class representing the interface for a figure on a chart
class (Eq a, Trans a) => Figure a where
  {-# MINIMAL isTrivial, refPoint, box #-}

  -- | Returns `True` is figure is trivial in a certain sence.
  isTrivial :: a -> Bool

  -- | Returns `True` is figure is not trivial in a certain sence.
  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

  -- | Returns a refference point using for superposing and figure locataion.
  refPoint :: a -> CN

  -- | The geometric similarity relation.
  isSimilar :: a -> a -> Bool
  isSimilar = (==)

  box :: a -> Box

instance Bounded Double where
  minBound = -1/0
  maxBound = 1/0

width :: Figure f => f -> Double
width f = let ((Min xmin, Min ymin), (Max xmax, Max ymax)) = box f
          in abs $ xmax - xmin

height :: Figure f => f -> Double
height f = let ((Min xmin, Min ymin), (Max xmax, Max ymax)) = box f
           in abs $ ymax - ymin

corner p = ((x1:+y2, x2:+y2),(x1:+y1, x2:+y1))
    where ((Min x1,Min y1),(Max x2, Max y2)) = box p

lower = snd
upper = fst
right = snd
left  = fst

------------------------------------------------------------
