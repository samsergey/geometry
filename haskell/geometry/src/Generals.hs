module Generals where

import Data.Fixed (mod')
import Data.Complex

type Number = Double
type XY = Complex Number

tolerance = 1e-10

data Directed = Ang Number | Vec XY
  deriving Show

instance Eq Directed where
  a == b = abs (toRad a - toRad b) < tolerance

instance Ord Directed where
  a <= b = a == b || toRad a < toRad b

toAng (Ang a) = Ang a
toAng (Vec v) = Ang $ (180/pi * phase v) `mod'` 360

toVec (Vec x) = Vec x
toVec (Ang a) = Vec $ mkPolar 1 (a/180*pi)

toRad (Vec v) = phase v `mod'` (2*pi)
toRad (Ang a) = (a*pi/180) `mod'` (2*pi)

toTurns = (/ (2*pi)) . toRad

instance Num Directed where
  fromInteger n = Ang $ fromIntegral n `mod'` 360
  (+) = withAng2 (+)
  (*) = withAng2 (*)
  negate = withAng negate
  abs = withAng abs
  signum = withAng signum

withAng op a = let Ang a' = toAng a
               in Ang $ op a' `mod'` 360 
                  
withAng2 op a b = let Ang a' = toAng a
                      Ang b' = toAng b
                  in Ang $ (a' `op` b') `mod'` 360 
