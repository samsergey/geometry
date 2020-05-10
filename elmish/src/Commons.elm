module Commons exposing (..)

signum x = if x < 0
           then -1
           else if x == 0
                then 0
                else 1
           
sub (x,y) (a,b) = (x-a, y-b)
add (x,y) (a,b) = (x+a, y+b)
scl a (x,y)     = (x*a, y*a)
dot (x,y) (a,b) = x*a + y*b
cross (x,y) (a,b) = x*b - y*a
norm (x,y) = sqrt (x*x + y*y)
normalize v = if isNullVector v then v else scl (1 / norm v) v 

rad2deg x = if (x >= 0)
            then 180*x/pi
            else 360 + 180*x/pi

type Angular = A Float | V (Float, Float)

toAngle v = case v of
              V (x, y) -> A <| rad2deg <| atan2 y x
              a -> a

toVector x = case x of
               A a -> let phi = radians a
                      in V (cos phi, sin phi)
               v -> v

toTurns x = case x of
              A a -> a / 360
              v -> toTurns <| toAngle v
         


type alias XY = (Float, Float)
  
type alias Curve t = {t | curve : { point : Float -> XY
                                  , locus : XY -> Float
                                  , tangent : Float -> XY } }

  
nullVector : XY
nullVector = (0, 0)

origin : XY
origin = (0, 0)
             
isNullVector (x, y) = x == 0 && y == 0

trivialCurve = { point = \_ -> origin
               , locus = \_ -> 0
               , tangent = \_ -> nullVector }

