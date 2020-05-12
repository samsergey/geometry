{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Transform where

import Data.Complex

import Base

------------------------------------------------------------

type TMatrix = ((Number, Number, Number),(Number, Number, Number))

class Trans a where
  transform :: TMatrix -> a -> a

  transformAt :: Pos p => p -> (a -> a) -> a -> a
  transformAt p t = translate (xy) . t . translate (-xy)
    where xy = pos p
  
  translate :: Pos p => p -> a -> a
  translate = transform . translateT . pos

  scale :: Number -> a -> a
  scale = transform . scaleT

  scaleAt :: Pos p => p -> Number -> a -> a
  scaleAt p s = transformAt p (scale s)

  rotate :: Dir -> a -> a
  rotate = transform . rotateT . toRad

  rotateAt :: Pos p => p -> Dir -> a -> a
  rotateAt p a = transformAt p (rotate a)
         
  reflect :: Dir -> a -> a
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

class Trans a => Pos a where

  pos :: a -> CXY
  pos p = let (x, y) = coord p in x :+ y
  
  coord :: a -> (Number, Number)
  coord p = let x :+ y = pos p in (x, y)

  dot :: a -> a -> Number
  dot a b = let (xa, ya) = coord a
                (xb, yb) = coord b
                in xa*xb + ya*yb

  dir :: a -> a -> Dir
  dir p1 p2 = Vec (pos p2 - pos p1)

  cross :: a -> a -> Number
  cross a b = let (xa, ya) = coord a
                  (xb, yb) = coord b
              in xa*yb - ya*xb

  norm :: a -> Number
  norm = magnitude . pos

  normalize :: a -> a
  normalize v
    | pos v == 0 = v
    | otherwise = scale (1/norm v) v


instance Trans CXY where
  transform t a  = transformCXY t a

instance Pos CXY where
  pos = id

instance Trans XY where
  transform t a  = coord $ transformCXY t $ pos a

instance Pos XY where
  coord = id

------------------------------------------------------------

data Location = Inside | Outside | OnCurve deriving (Show, Eq)

class Curve a where
  param :: a -> Number -> CXY
  locus :: Pos p => a -> p -> Number
  length :: a -> Number

  tangent :: a -> Number -> Dir
  tangent f t = normal f t + 90

  isClosed :: a -> Bool
  isClosed _ = False
  
  location :: Pos p => p -> a -> Location
  location _ _ = Outside
  
  normal :: a -> Number -> Dir
  normal f t = 90 + tangent f t


isContaining :: (Curve c, Pos p) => c -> p -> Bool
isContaining c p = location p c == OnCurve
  
isEnclosing :: (Curve c, Pos p) => c -> p -> Bool
isEnclosing c p = location p c == Inside


class Curve a => Linear a where
  start :: a -> CXY
  end :: a -> CXY
  vector :: a -> CXY

  unit :: a -> Number
  unit = magnitude . vector

  angle :: a -> Dir
  angle = Vec . vector
