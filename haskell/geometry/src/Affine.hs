{-# Language FlexibleInstances #-}

module Affine where

import Data.Complex

import Base

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

  reflectAt :: Linear l => l -> a -> a
  reflectAt l = transformAt (pivot l) (reflect (angle l))

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
  transform t  = Cmp . cmp . transformXY t . coord

 
------------------------------------------------------------

class Trans a => Affine a where
  {-# MINIMAL (fromCN | fromCoord), (cmp | coord) #-}

  fromCN :: CN -> a
  fromCN = fromCoord . coord

  fromCoord :: XY -> a
  fromCoord = fromCN . cmp
    
  cmp :: a -> CN
  cmp p = let (x, y) = coord p in x :+ y
  
  coord :: a -> XY
  coord p = let x :+ y = cmp p in (x, y)

  dot :: a -> a -> Double
  dot a b = let (xa, ya) = coord a
                (xb, yb) = coord b
                in xa*xb + ya*yb

  orthogonal :: a -> a -> Bool
  orthogonal  a b = a `dot` b ~== 0

  collinear :: a -> a -> Bool
  collinear a b = a `cross` b ~== 0

  azimuth :: a -> a -> Angular
  azimuth p1 p2 = Cmp (cmp p2 - cmp p1)

  cross :: a -> a -> Double
  cross a b = let (xa, ya) = coord a
                  (xb, yb) = coord b
              in xa*yb - ya*xb

  norm :: a -> Double
  norm = magnitude . cmp

  distance :: a -> a -> Double
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
  angle = deg . Cmp . cmp

------------------------------------------------------------

instance Affine CN where
  cmp = id
  fromCN = id


instance Affine XY where
  coord = id
  fromCoord = id


instance Affine Angular where
  cmp (Cmp v) = v
  cmp d = mkPolar 1 (rad d)
  fromCN = Cmp

------------------------------------------------------------

class Affine a => Linear a where
  pivot :: a -> CN

  vector :: a -> CN
  vector = normalize . cmp

------------------------------------------------------------

data Location = Inside | Outside | OnCurve deriving (Show, Eq)

class Curve a where
  {-# MINIMAL param, locus, unit, (normal | tangent)  #-}
  param :: a -> Double -> CN
  locus :: Affine p => a -> p -> Double
  unit  :: a -> Double

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
