{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
module Base where

import Data.Fixed (mod')
import Data.Complex
import Data.List
import Control.Applicative
import Data.Monoid
import Data.Maybe

------------------------------------------------------------

type CN = Complex Double
type XY = (Double, Double)

------------------------------------------------------------

infixl 5 <|
(<|) = flip ($)

------------------------------------------------------------

infix 4 ~==

class AlmostEq a where
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
a ~<= b = a ~== b || a < b

infix 4 ~>=
a ~>= b = a ~== b || a > b

------------------------------------------------------------

newtype Angular = Angular Double

asRad = Angular . (`mod'` (2*pi))
asDeg a = asRad $ a / 180*pi
asCmp a = asRad $ phase a
asTurns a = asRad $ a*2*pi

deg (Angular a) = (a * 180 / pi) `mod'` 360
rad (Angular a) = a  `mod'` (2*pi)
turns (Angular a) = (a / (2*pi))  `mod'` 1

instance Show Angular where
  show a = show (deg a) <> "°"

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

withAngular op a = asRad $ op (rad a)
withAngular2 op a b = asRad (rad a `op` rad b)

------------------------------------------------------------

both f (a,b) = (f a, f b)

------------------------------------------------------------

type TMatrix = ((Double, Double, Double),(Double, Double, Double))

class Trans a where
  {-# MINIMAL transform #-}
  transform :: TMatrix -> a -> a

  transformAt :: Affine p => p -> (a -> a) -> a -> a
  transformAt p t = translate xy . t . translate (-xy)
    where xy = cmp p
  
  translate :: Affine p => p -> a -> a
  translate = transform . translateT . cmp

  scale :: Double -> a -> a
  scale = transform . scaleT

  scaleAt :: Affine p => p -> Double -> a -> a
  scaleAt p s = transformAt p (scale s)

  rotate :: Angular -> a -> a
  rotate = transform . rotateT . rad

  rotateAt :: Affine p => p -> Angular -> a -> a
  rotateAt p a = transformAt p (rotate a)
         
  reflect :: Angular -> a -> a
  reflect d = transform $ reflectT $ rad d

  superpose :: (Affine p1, Affine p2) => p1 -> p2 -> a -> a
  superpose p1 p2 = translate (cmp p2 - cmp p1)


transformCN :: TMatrix -> CN -> CN
transformCN t = cmp . transformXY t . coord

transformXY :: TMatrix -> XY -> XY
transformXY ((a11, a12, sx), (a21, a22, sy)) (x, y) =
    (a12*y + a11*x + sx, a22*y + a21*x + sy)

rotateT :: Double -> TMatrix
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

reflectT :: Double -> TMatrix
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

translateT :: CN -> TMatrix
translateT (dx :+ dy) = ((1, 0, dx), (0, 1, dy))

scaleT :: Double -> TMatrix
scaleT  a = ((a, 0, 0), (0, a, 0))

------------------------------------------------------------

instance Trans CN where
  transform t = cmp . transformXY t . coord

instance Trans XY where
  transform  = transformXY

instance Trans Angular where
  transform t  = asCmp . cmp . transformXY t . coord

 
------------------------------------------------------------

class Trans a => Affine a where
  {-# MINIMAL (fromCN | fromCoord), (cmp | coord) #-}

  fromCN :: CN -> a
  fromCN = fromCoord . coord

  fromCoord :: XY -> a
  fromCoord = fromCN . cmp

  getX :: a -> Double
  getX = fst . coord
  
  getY :: a -> Double
  getY = snd . coord
    
  cmp :: a -> CN
  cmp p = let (x, y) = coord p in x :+ y
  
  coord :: a -> XY
  coord p = let x :+ y = cmp p in (x, y)

  dot :: a -> a -> Double
  dot a b = let (xa, ya) = coord a
                (xb, yb) = coord b
                in xa*xb + ya*yb

  isOrthogonal :: a -> a -> Bool
  isOrthogonal  a b = a `dot` b ~== 0

  isOpposite :: a -> a -> Bool
  isOpposite a b = cmp a + cmp b ~== 0

  isCollinear :: a -> a -> Bool
  isCollinear a b = a `cross` b ~== 0

  azimuth :: a -> a -> Angular
  azimuth p1 p2 = asCmp (cmp p2 - cmp p1)

  det :: Affine b => (a, b) -> Double
  det (a, b) = let (xa, ya) = coord a
                   (xb, yb) = coord b
               in xa*yb - ya*xb

  cross :: a -> a -> Double
  cross a b = det (a, b)

  norm :: a -> Double
  norm = magnitude . cmp

  distance :: Affine b => a -> b -> Double
  distance a b = magnitude (cmp a - cmp b)

  normalize :: a -> a
  normalize v
    | cmp v == 0 = v
    | otherwise = scale (1/norm v) v

  roundUp :: Double -> a -> a
  roundUp d = fromCoord . (\(x,y) -> (rounding x, rounding y)) . coord
    where rounding x = fromIntegral (ceiling (x /d)) * d

  isZero :: a -> Bool
  isZero a = cmp a ~== 0

  angle :: a -> Angular
  angle = asCmp . cmp

  transpose :: (a, a) -> (a, a)
  transpose (a, b) = ( fromCoord (getX a, getX b)
                     , fromCoord (getY a, getY b))

    
infix 8 .@
(.@) :: Curve a => a -> Double -> CN
c .@ x = param c x

infix 8 @.
(@.) :: (Curve a, Affine p) => p -> a -> Double
p @. c  = locus c p

infix 8 .?
(.?) :: Curve a => a -> Double -> Maybe CN
c .? x = maybeParam c x

infix 8 ?.
(?.) :: (Curve a, Affine p) => p -> a -> Maybe Double
p ?. c  = maybeLocus c p

------------------------------------------------------------

instance Affine CN where
  cmp = id
  fromCN = id


instance Affine XY where
  coord = id
  fromCoord = id


instance Affine Angular where
  cmp a = mkPolar 1 (rad a)
  fromCN = asCmp . normalize

------------------------------------------------------------

data Location = Inside | Outside | OnCurve deriving (Show, Eq)

class Curve a where
  {-# MINIMAL (param | maybeParam),
              (locus | maybeLocus),
              (normal | tangent),
              distanceTo #-}
  
  param :: a -> Double -> CN
  param c x = fromMaybe 0 $ maybeParam c x

  maybeParam :: a -> Double -> Maybe CN
  maybeParam c x =
    let p = param c x
    in if c `isContaining` p then Just p else Nothing                  

  locus :: Affine p => a -> p -> Double
  locus c p =  fromMaybe 0 $ maybeLocus c p 

  maybeLocus :: Affine p => a -> p -> Maybe Double
  maybeLocus c p = 
    let x = locus c p
    in if c `isContaining` param c x
       then Just x
       else Nothing                  

  distanceTo :: Affine p => a -> p -> Double
    
  start :: a -> CN
  start c = c `param` 0
  
  unit :: a -> Double
  unit _ = 1

  tangent :: a -> Double -> Angular
  tangent f t = normal f t + 90
  
  normal :: a -> Double -> Angular
  normal f t = 90 + tangent f t

  isClosed :: a -> Bool
  isClosed _ = False
  
  location :: Affine p => p -> a -> Location
  location _ _ = Outside
  
  isContaining :: Affine p => a -> p -> Bool
  isContaining c p = location p c == OnCurve
  
  isEnclosing :: Affine p => a -> p -> Bool
  isEnclosing c p = location p c == Inside 

------------------------------------------------------------

class (Curve a, Curve b) => Intersections a b where
  intersections :: a -> b -> [CN]

  isIntersecting :: a -> b -> Bool
  isIntersecting a b = not . null $ intersections a b

------------------------------------------------------------

newtype Corner = Corner (Endo (Int, Int))
  deriving (Semigroup, Monoid)

lower =  Corner . Endo $ \(_, x) -> (-1, x)
upper =  Corner . Endo $ \(_, x) -> (1, x)
middleX =   Corner . Endo $ \(_, x) -> (0, x)
left =  Corner . Endo $ \(x, _) -> (x, -1)
right =  Corner . Endo $ \(x, _) -> (x, 1)
middleY =   Corner . Endo $ \(x, _) -> (x, 0)
middle = middleX <> middleY
corner (Corner c) = appEndo c (-1, -1)
cornerX = fst . corner
cornerY = snd . corner

------------------------------------------------------------

class Trans a => Figure a where
  isTrivial :: a -> Bool
  isSimilar :: a -> a -> Bool

  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

  refPoint :: a -> CN

  labelPosition :: a -> CN
  labelPosition = refPoint
  
  labelOffset :: a -> XY
  labelOffset _ = (0.5, 0.5)
  
  labelCorner :: a -> (Int, Int)
  labelCorner f = let (x, y) = labelOffset f
                  in (signum (round x), signum (round y))

------------------------------------------------------------

newtype Labeled a = Labeled ((String, First (Int, Int), First XY), a)
  deriving Functor


instance Show a => Show (Labeled a) where
  show (Labeled ((s,_,_), x)) = s <> ":" <> show x


instance Applicative Labeled where
  pure x = Labeled (mempty, x)
  Labeled (l1, f) <*> Labeled (l2, x) = Labeled (l1 <> l2, f x)


fromLabeled    (Labeled (_, x)) = x
getLabel       (Labeled ((l, _, _), _)) = l
getLabelCorner (Labeled ((_, First c, _), _)) = c
getLabelOffset (Labeled ((_, _, First o), _)) = o
appLabel l = Labeled (l, id)


label l x = appLabel (l, mempty, mempty) <*> pure x
lcorn p x  = appLabel (mempty, pure p, mempty) <*> x
lpos d x  = appLabel (mempty, mempty, pure d) <*> x


withLabeled f l = fromLabeled $ f <$> l
withLabeled2 f (Labeled (l, a)) = f a


instance Eq a => Eq (Labeled a) where
  a == b = fromLabeled $ (==) <$> a <*> b

instance Trans a => Trans (Labeled a) where
  transform t x = transform t <$> x

instance Affine a => Affine (Labeled a) where
  cmp  = withLabeled cmp
  fromCN c = pure (fromCN c)

instance Curve a => Curve (Labeled a) where
  param = withLabeled2 param
  locus = withLabeled2 locus
  normal = withLabeled2 normal
  distanceTo = withLabeled2 distanceTo

instance Figure a => Figure (Labeled a) where
   refPoint = withLabeled refPoint
   isTrivial = withLabeled isTrivial
   isSimilar a b = fromLabeled $ isSimilar <$> a <*> b
   labelPosition = withLabeled labelPosition
   labelOffset lf = labelOffset (fromLabeled lf) `fromMaybe` getLabelOffset lf
   labelCorner lf = labelCorner (fromLabeled lf) `fromMaybe` getLabelCorner lf

------------------------------------------------------------
