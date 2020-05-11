module Transform where

import Data.Complex

import Generals
import Curve

type TMatrix = ((Number, Number, Number),(Number, Number, Number))

class Trans a where
  transform :: TMatrix -> a -> a

  transformAt :: XY -> (a -> a) -> a -> a
  transformAt xy t = translate xy . t . translate (-xy)
  
  translate :: XY -> a -> a
  translate = transform . translateT

  scale :: Number -> a -> a
  scale = transform . scaleT

  scaleAt :: XY -> Number -> a -> a
  scaleAt p s = transformAt p (scale s)

  rotate :: Directed -> a -> a
  rotate = transform . rotateT . toRad

  rotateAt :: XY -> Directed -> a -> a
  rotateAt p a = transformAt p (rotate a)
         
  reflect :: Directed -> a -> a
  reflect d = transform $ reflectT $ toRad d

  reflectAt :: Linear l => l -> a -> a
  reflectAt l = transformAt (start l) (reflect (angle l))


transformXY :: TMatrix -> XY -> XY
transformXY ((a11, a12, sx), (a21, a22, sy)) (x :+ y) =
    (a12*y + a11*x + sx) :+ (a22*y + a21*x + sy)

rotateT :: Number -> TMatrix
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

reflectT :: Number -> TMatrix
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

translateT :: XY -> TMatrix
translateT (dx :+ dy) = ((1, 0, dx), (0, 1, dy))

scaleT :: Number -> TMatrix
scaleT  a = ((a, 0, 0), (0, a, 0))
