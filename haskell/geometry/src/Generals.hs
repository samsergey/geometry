module Generals where

type Number = Float
type XY = (Number, Number)

tolerance = 1e-5

data Directed = Ang Number | Vec XY
  deriving Show

instance Eq Directed where
  a == b = abs (toRad a - toRad b) < tolerance

instance Ord Directed where
  a <= b = a == b || toRad a < toRad b

toAng (Ang a) = Ang a
toAng (Vec (x, y)) = let a = 180/pi * atan2 y x
                     in Ang $ if a < 0 then 360+a else a

toVec (Vec x) = Vec x
toVec d = let phi = toRad d in Vec (cos phi, sin phi)

toRad d = let Ang a = toAng d in a*pi/180

toTurns = (/ (2*pi)) . toRad

rot90 d = let Vec (x, y) = toVec d in Vec (-y, x)

instance Num Directed where
  fromInteger n = Ang $ fromIntegral $ n `mod` 360
  (+) = withAng2 (+)
  (*) = withAng2 (*)
  negate = withAng negate
  abs = withAng abs
  signum = withAng signum

withAng op a = let Ang a' = toAng a
               in toAng $ toVec $ Ang $ op a'
                  
withAng2 op a b = let Ang a' = toAng a
                      Ang b' = toAng b
                  in toAng $ toVec $Ang $ (a' `op` b')
