module Transform where

import Generals
import Curve

type TMatrix = ((Float, Float, Float),(Float, Float, Float))

class Trans a where
  transform :: TMatrix -> a -> a

  transformAt :: XY -> (a -> a) -> a -> a
  transformAt (x,y) t = translate (x, y) . t . translate (-x, -y)
  
  translate :: XY -> a -> a
  translate = transform . translateT

  scale :: Float -> a -> a
  scale = transform . scaleT

  scaleAt :: XY -> Float -> a -> a
  scaleAt p s = transformAt p (scale s)

  rotate :: Directed -> a -> a
  rotate = transform . rotateT . toRad

  rotateAt :: XY -> Directed -> a -> a
  rotateAt p a = transformAt p (rotate a)
         
  reflect :: Directed -> a -> a
  reflect d = transform $ reflectT $ toRad d

  reflectAt :: Linear l => l -> a -> a
  reflectAt l = transformAt (start l) (reflect (angle l))


transformXY :: TMatrix -> (Float, Float) -> (Float, Float)
transformXY ((a11, a12, sx), (a21, a22, sy)) (x, y) =
    (a12*y + a11*x + sx, a22*y + a21*x + sy)

rotateT :: Float -> TMatrix
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

reflectT :: Float -> TMatrix
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

translateT :: XY -> TMatrix
translateT (dx, dy) = ((1, 0, dx), (0, 1, dy))

scaleT :: Float -> TMatrix
scaleT  a = ((a, 0, 0), (0, a, 0))
