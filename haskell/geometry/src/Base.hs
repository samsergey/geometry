{-# Language FlexibleInstances, UndecidableInstances #-}
{-# Language MultiParamTypeClasses #-}
module Base where

import Data.Fixed (mod')
import Data.Complex
import Data.List

------------------------------------------------------------

type Number = Double
type CXY = Complex Number
type XY = (Number, Number)

------------------------------------------------------------

infix 4 ~==

class AEq a where
  (~==) :: a -> a -> Bool

instance AEq Int where a ~== b = a == b
instance AEq Integer where  a ~== b = a == b

instance AEq Double where
  a ~== b = abs (a - b) < 1e-10 || abs (a-b) < 1e-10 * abs(a+b)

instance (RealFloat a, Ord a, Fractional a, Num a, AEq a) => AEq (Complex a) where
  a ~== b = magnitude (a - b) < 1e-10 || magnitude (a-b) < 1e-10 * magnitude(a+b)

instance (AEq a, AEq b) => AEq (a, b) where
  (a1,b1) ~== (a2,b2) = a1 ~== a2 && b1 ~== b2

------------------------------------------------------------

data Dir = Ang Number | Vec CXY
  deriving Show

instance AEq Dir where  a ~== b = toRad a ~== toRad b

instance Eq Dir  where  a == b = a ~== b

instance Ord Dir where  a <= b = a == b || toRad a < toRad b


toAng (Ang a) = Ang a
toAng (Vec v) | v ~== 0 = Ang 0
              | otherwise = Ang $ (180/pi * phase v) `mod'` 360

toVec (Vec x) = Vec x
toVec (Ang a) = Vec $ mkPolar 1 (a/180*pi)

toRad (Vec v) = if v ~== 0 then 0 else phase v `mod'` (2*pi)
toRad (Ang a) = (a*pi/180) `mod'` (2*pi)

toTurns = (/ (2*pi)) . toRad

instance Num Dir where
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


------------------------------------------------------------

class Figure a where
  isTrivial :: a -> Bool
  isSimilar :: a -> a -> Bool

