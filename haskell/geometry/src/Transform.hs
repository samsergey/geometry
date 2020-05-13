{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Transform where

import Data.Complex
import Test.QuickCheck (Arbitrary(..))

import Base

------------------------------------------------------------

type TMatrix = ((Number, Number, Number),(Number, Number, Number))

class Trans a where
  {-# MINIMAL transform #-}
  transform :: TMatrix -> a -> a

  transformAt :: Directed p => p -> (a -> a) -> a -> a
  transformAt p t = translate (xy) . t . translate (-xy)
    where xy = pos p
  
  translate :: Directed p => p -> a -> a
  translate = transform . translateT . pos

  scale :: Number -> a -> a
  scale = transform . scaleT

  scaleAt :: Directed p => p -> Number -> a -> a
  scaleAt p s = transformAt p (scale s)

  rotate :: Angular -> a -> a
  rotate = transform . rotateT . toRad

  rotateAt :: Directed p => p -> Angular -> a -> a
  rotateAt p a = transformAt p (rotate a)
         
  reflect :: Angular -> a -> a
  reflect d = transform $ reflectT $ toRad d

  reflectAt :: Linear l => l -> a -> a
  reflectAt l = transformAt (start l) (reflect (angle l))


transformCXY :: TMatrix -> CXY -> CXY
transformCXY ((a11, a12, sx), (a21, a22, sy)) (x :+ y) =
    (a12*y + a11*x + sx) :+ (a22*y + a21*x + sy)

rotateT :: Number -> TMatrix
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

reflectT :: Number -> TMatrix
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

translateT :: CXY -> TMatrix
translateT (dx :+ dy) = ((1, 0, dx), (0, 1, dy))

scaleT :: Number -> TMatrix
scaleT  a = ((a, 0, 0), (0, a, 0))

------------------------------------------------------------

class Trans a => Directed a where
  {-# MINIMAL (fromPos | fromCoord), (pos | coord) #-}

  fromPos :: CXY -> a
  fromPos = fromCoord . coord

  fromCoord :: XY -> a
  fromCoord = fromPos . pos
    
  pos :: a -> CXY
  pos p = let (x, y) = coord p in x :+ y
  
  coord :: a -> (Number, Number)
  coord p = let x :+ y = pos p in (x, y)

  dot :: a -> a -> Number
  dot a b = let (xa, ya) = coord a
                (xb, yb) = coord b
                in xa*xb + ya*yb

  perp :: a -> a -> Bool
  perp  a b = a `dot` b ~== 0

  collinear :: a -> a -> Bool
  collinear a b = a `cross` b ~== 0

  dir :: a -> a -> Angular
  dir p1 p2 = Vec (pos p2 - pos p1)

  cross :: a -> a -> Number
  cross a b = let (xa, ya) = coord a
                  (xb, yb) = coord b
              in xa*yb - ya*xb

  norm :: a -> Number
  norm = magnitude . pos

  distance :: a -> a -> Number
  distance a b = magnitude (pos a - pos b)

  normalize :: a -> a
  normalize v
    | pos v == 0 = v
    | otherwise = scale (1/norm v) v

  roundUp :: Number -> a -> a
  roundUp d = fromCoord . (\(x,y) -> (rounding x, rounding y)) . coord
    where rounding x = fromIntegral (floor (x /d)) * d

  isZero :: a -> Bool
  isZero a = pos a ~== 0


instance Trans CXY where
  transform  = transformCXY

instance Directed CXY where
  pos = id
  fromPos = id

instance Trans XY where
  transform t  = coord . transformCXY t . pos

instance Directed XY where
  coord = id
  fromCoord = id

instance Trans Angular where
  transform t  = Vec . transformCXY t . pos

instance Directed Angular where
  pos d = let Vec v = toVec d in v
  fromPos = Vec


------------------------------------------------------------

newtype Position a = Position {getPosition :: a}
  deriving Show

instance Trans a => Trans (Position a) where
  transform t (Position p) = Position (transform t p)
  
instance Directed a => Directed (Position a) where
  pos = pos . getPosition
  fromPos = Position . fromPos

instance (Trans a, Directed a, Arbitrary a) => Arbitrary (Position a) where
  arbitrary = Position <$> arbitrary
  shrink = shrinkPos 1

shrinkPos :: (Trans a, Directed a) => Number -> a -> [a]
shrinkPos d x = map (roundUp d) $
                takeWhile (\p -> distance x p >= d/2) $
                map (`Transform.scale` x) $
                map (1 -) $
                iterate (/2) 1

------------------------------------------------------------

data Location = Inside | Outside | OnCurve deriving (Show, Eq)

class Curve a where
  {-# MINIMAL param, locus, length, (normal | tangent)  #-}
  param :: a -> Number -> CXY
  locus :: Directed p => a -> p -> Number
  length :: a -> Number

  tangent :: a -> Number -> Angular
  tangent f t = normal f t + 90

  isClosed :: a -> Bool
  isClosed _ = False
  
  location :: Directed p => p -> a -> Location
  location _ _ = Outside
  
  normal :: a -> Number -> Angular
  normal f t = 90 + tangent f t

  isContaining :: Directed p => a -> p -> Bool
  isContaining c p = location p c == OnCurve
  
  isEnclosing :: Directed p => a -> p -> Bool
  isEnclosing c p = location p c == Inside


class Curve a => Linear a where
  {-# MINIMAL start, end, vector #-}
  start :: a -> CXY
  end :: a -> CXY
  vector :: a -> CXY

  unit :: a -> Number
  unit = magnitude . vector

  angle :: a -> Angular
  angle = Vec . vector

------------------------------------------------------------

-- class Angularected a where
--   isCollinear :: a -> a -> Bool
--   isPerpendicular :: a -> a -> Bool
--   polar
--   cartesian
--   dot
--   cross
--   norm
--   normalized
--   angle
  
  
  
