module Transform exposing (..)

type alias Transform = ((Float, Float, Float), (Float, Float, Float))

transformV : Transform -> (Float, Float) -> (Float, Float)
transformV ((a11, a12, sx), (a21, a22, sy)) (x, y) =
    (a12*y + a11*x + sx, a22*y + a21*x + sy)

rotateT : Float -> Transform
rotateT a = ((cos a, -(sin a), 0), (sin a, cos a, 0))

reflectT : Float -> Transform
reflectT a = ((cos (2*a), sin (2*a), 0), (sin (2*a), -(cos (2*a)), 0))

translateT : (Float, Float) -> Transform
translateT (dx, dy) = ((1, 0, dx), (0, 1, dy))

scaleT : Float -> Float -> Transform
scaleT  a  b = ((a, 0, 0), (0, b, 0))
