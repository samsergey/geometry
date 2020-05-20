{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# language DeriveAnyClass #-}

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

infixl 5 %
(%) = flip ($)

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

rotateT :: Double -> TMatrix
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

reflectT :: Double -> TMatrix
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

translateT :: CN -> TMatrix
translateT (dx :+ dy) = ((1, 0, dx), (0, 1, dy))

scaleT :: Double -> Double -> TMatrix
scaleT x y = ((x, 0, 0), (0, y, 0))

transformCN :: TMatrix -> CN -> CN
transformCN t = cmp . transformXY t . coord

transformXY :: TMatrix -> XY -> XY
transformXY ((a11, a12, sx), (a21, a22, sy)) (x, y) =
    (a12*y + a11*x + sx, a22*y + a21*x + sy)



transformAt :: (Trans a, Affine p) => p -> (a -> a) -> a -> a
transformAt p t = translate xy . t . translate (-xy)
  where xy = cmp p
  
translate :: (Trans a, Affine p) => p -> a -> a
translate = transform . translateT . cmp

scale :: Trans a => Double -> a -> a
scale s = transform (scaleT s s)

scaleX :: Trans a => Double -> a -> a
scaleX s = transform (scaleT s 1)

scaleY :: Trans a => Double -> a -> a
scaleY s = transform (scaleT 1 s)

scaleAt :: (Trans a, Affine p) => p -> Double -> a -> a
scaleAt p s = transformAt p (scale s)

scaleXAt :: (Trans a, Affine p) => p -> Double -> a -> a
scaleXAt p s = transformAt p (scaleX s)

scaleYAt :: (Trans a, Affine p) => p -> Double -> a -> a
scaleYAt p s = transformAt p (scaleY s)

rotate :: Trans a => Angular -> a -> a
rotate = transform . rotateT . rad

rotateAt :: (Trans a, Affine p) => p -> Angular -> a -> a
rotateAt p a = transformAt p (rotate a)
         
reflect :: Trans a => Angular -> a -> a
reflect d = transform $ reflectT $ rad d

superpose :: (Trans a, Affine p1, Affine p2) => p1 -> p2 -> a -> a
superpose p1 p2 = translate (cmp p2 - cmp p1)


------------------------------------------------------------

instance Trans CN where
  transform t = cmp . transformXY t . coord

instance Trans XY where
  transform  = transformXY

instance Trans Angular where
  transform t  = asCmp . cmp . transformXY t . coord
 
------------------------------------------------------------

class Affine a where
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

dot :: (Affine a, Affine b) => a -> b -> Double
dot a b = let (xa, ya) = coord a
              (xb, yb) = coord b
              in xa*xb + ya*yb

isOrthogonal :: (Affine a, Affine b) => a -> b -> Bool
isOrthogonal  a b = cmp a `dot` cmp b ~== 0

isOpposite :: (Affine a, Affine b) => a -> b -> Bool
isOpposite a b = cmp a + cmp b ~== 0

isCollinear :: (Affine a, Affine b) => a -> b -> Bool
isCollinear a b = cmp a `cross` cmp b ~== 0

azimuth :: (Affine a, Affine b) => a -> b -> Angular
azimuth p1 p2 = asCmp (cmp p2 - cmp p1)

det :: (Affine a, Affine b) => (a, b) -> Double
det (a, b) = let (xa, ya) = coord a
                 (xb, yb) = coord b
             in xa*yb - ya*xb

cross :: (Affine a, Affine b) => a -> b -> Double
cross a b = det (a, b)

norm :: Affine a => a -> Double
norm = magnitude . cmp

distance :: (Affine a, Affine b) => a -> b -> Double
distance a b = magnitude (cmp a - cmp b)

normalize :: Affine a => a -> a
normalize v
  | cmp v == 0 = v
  | otherwise = fromCN $ (1/norm v :+ 0) * cmp v

roundUp :: Affine a => Double -> a -> a
roundUp d = fromCoord . (\(x,y) -> (rounding x, rounding y)) . coord
  where rounding x = fromIntegral (ceiling (x /d)) * d

isZero :: Affine a => a -> Bool
isZero a = cmp a ~== 0

angle :: Affine a => a -> Angular
angle = asCmp . cmp

transpose :: Affine a => (a, a) -> (a, a)
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
              distanceTo,
              (location | (isContaining, isEnclosing)) #-}
  
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
    
  unit :: a -> Double
  unit _ = 1

  tangent :: a -> Double -> Angular
  tangent f t = normal f t + 90
  
  normal :: a -> Double -> Angular
  normal f t = 90 + tangent f t

  isClosed :: a -> Bool
  isClosed _ = False
  
  location :: Affine p => p -> a -> Location
  location p c | isContaining c p = OnCurve
               | isClosed c && isEnclosing c p = Inside
               | otherwise = Outside
  
  isContaining :: Affine p => a -> p -> Bool
  isContaining c p = location p c == OnCurve
  
  isEnclosing :: Affine p => a -> p -> Bool
  isEnclosing c p = location p c == Inside 

start :: Curve a => a -> CN
start c = c .@ 0


------------------------------------------------------------

class (Curve a, Curve b) => Intersections a b where
  intersections :: a -> b -> [CN]

isIntersecting :: Intersections a b => a -> b -> Bool
isIntersecting a b = not . null $ intersections a b

------------------------------------------------------------

-- newtype Corner = Corner (Endo (Int, Int))
--   deriving (Semigroup, Monoid)

-- lower =  Corner . Endo $ \(_, x) -> (-1, x)
-- upper =  Corner . Endo $ \(_, x) -> (1, x)
-- middleX =   Corner . Endo $ \(_, x) -> (0, x)
-- left =  Corner . Endo $ \(x, _) -> (x, -1)
-- right =  Corner . Endo $ \(x, _) -> (x, 1)
-- middleY =   Corner . Endo $ \(x, _) -> (x, 0)
-- middle = middleX <> middleY
-- corner (Corner c) = appEndo c (-1, -1)
-- cornerX = fst . corner
-- cornerY = snd . corner

------------------------------------------------------------

data LabelSettings = LabelSettings
  { getLabel :: Last String
  , getLabelCorner :: Last (Int, Int)
  , getLabelOffset :: Last XY
  , getLabelPosition :: Last CN
  , getLabelAngle :: Last Angular} deriving (Show)


instance Semigroup LabelSettings where
  l1 <> l2 = LabelSettings
    { getLabel = getLabel l1 <> getLabel l2
    , getLabelCorner = getLabelCorner l1 <> getLabelCorner l2
    , getLabelOffset = getLabelOffset l1 <> getLabelOffset l2
    , getLabelPosition = getLabelPosition l1 <> getLabelPosition l2
    , getLabelAngle = getLabelAngle l1 <> getLabelAngle l2 }

instance Monoid LabelSettings where
  mempty = LabelSettings mempty mempty mempty mempty mempty

getLabelOption op = fromJust . getMaybeLabelOption op
getMaybeLabelOption op = getLast . op . (labelDefaults <> labelSettings)

------------------------------------------------------------

data Style = Style
  { getStroke :: Last String
  , getFill :: Last String
  , getDashing :: Last String
  , getStrokeWidth :: Last String } deriving (Show)

instance Semigroup Style where
  l1 <> l2 = Style
    { getStroke = getStroke l1 <> getStroke l2
    , getFill = getFill l1 <> getFill l2
    , getDashing = getDashing l1 <> getDashing l2
    , getStrokeWidth = getStrokeWidth l1 <> getStrokeWidth l2 }

instance Monoid Style where
  mempty = Style mempty mempty mempty mempty

getStyleOption op
  = getLast . op . (styleDefaults <> style)
  
------------------------------------------------------------

type Options = (LabelSettings, Style)

class (Eq a, Trans a) => Figure a where
  {-# MINIMAL isTrivial, refPoint, labelDefaults
            , options, setOptions #-}
  
  isTrivial :: a -> Bool
  refPoint :: a -> CN
  options :: a -> Options
  setOptions :: Options -> a -> a

  setLabel :: LabelSettings -> a -> a
  setLabel lb = setOptions (lb, mempty)

  setStyle :: Style -> a -> a
  setStyle s = setOptions (mempty, s)
  
  labelSettings :: a -> LabelSettings
  labelSettings = fst . options

  style :: a -> Style
  style = snd . options

  isSimilar :: a -> a -> Bool
  isSimilar = (==)
  
  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

  styleDefaults :: a -> Style
  styleDefaults _ = mempty

  labelDefaults :: a -> LabelSettings
  labelDefaults _ = mempty

  labelText :: a -> Maybe String
  labelText = getMaybeLabelOption getLabel

  labelPosition :: a -> CN
  labelPosition = getLabelOption getLabelPosition
  
  labelOffset :: a -> XY
  labelOffset = getLabelOption getLabelOffset
  
  labelCorner :: a -> (Int, Int)
  labelCorner f = let (x, y) = labelOffset f
                  in (signum (round x), signum (round y))

  labelAngle :: a -> Angular
  labelAngle = getLabelOption getLabelAngle


label :: Figure a => String -> a -> a
label l f = setLabel ld f
  where ld = (labelSettings f) { getLabel = pure l}


loffs :: Figure a => XY -> a -> a
loffs o f = setLabel ld f
  where ld = (labelSettings f) { getLabelOffset = pure o}


lpos :: (Affine p, Figure a) => p -> a -> a
lpos x f = setLabel ld f
  where ld = (labelSettings f) { getLabelPosition = pure (cmp x) }


lparam :: (Curve a, Figure a) => Double -> a -> a
lparam x f = setLabel ld f
  where ld = (labelSettings f) { getLabelPosition = pure (f .@ x) }
  
