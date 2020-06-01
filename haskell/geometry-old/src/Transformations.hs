module Transformations where

import Base

newtype Transformation = Transformation { tMatrix :: TMatrix }
  deriving Show

inv :: Transformation -> Transformation
inv (Transformation ((a, b, dx), (c, d, dy))) =
     Transformation ((d/det,-b/det,(b*dy-d*dx)/det),
                     (-c/det,a/det,(c*dx-a*dy)/det))
     where det = a*d-b*c

appTransformation :: Trans a => Transformation -> a -> a
appTransformation t = transform (tMatrix t)

instance Semigroup Transformation where
  t1 <> t2 = Transformation ((b1*c2+a1*a2, b1*d2+a1*b2, b1*dy2+a1*dx2+dx1),
                             (c2*d1+a2*c1, d1*d2+b2*c1, d1*dy2+dy1+c1*dx2))
    where ((a1, b1, dx1), (c1, d1, dy1)) = tMatrix t1
          ((a2, b2, dx2), (c2, d2, dy2)) = tMatrix t2

instance Monoid Transformation where
  mempty = Transformation ((1,0,0),(0,1,0))

infixl 5 %
(%) :: Trans a => a -> Transformation -> a
(%) = flip appTransformation 

t1 `inBasis` t2 = inv t2 <> t1 <> t2

t_translate :: Affine p => p -> Transformation
t_translate = Transformation . translateT . cmp

t_toOrigin f = (t_translate (negate (refPoint f)))

t_scale :: Double -> Transformation
t_scale s = Transformation $ scaleT s s

t_rotate :: Angular -> Transformation
t_rotate = Transformation . rotateT . rad
