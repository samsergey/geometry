{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
module Base where

import Data.Fixed (mod')
import Data.Complex
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Modifiers

------------------------------------------------------------

type CN = Complex Double
type XY = (Double, Double)

------------------------------------------------------------

infix 4 ~==

class AlmostEq a where
  (~==) :: a -> a -> Bool

instance AlmostEq Int where a ~== b = a == b
instance AlmostEq Integer where  a ~== b = a == b

instance AlmostEq Double where
  a ~== b = abs (a - b) < 1e-10 || abs (a-b) < 1e-10 * abs(a+b)

instance (RealFloat a, Ord a, Fractional a, Num a, AlmostEq a) => AlmostEq (Complex a) where
  a ~== b = magnitude (a - b) < 1e-10 || magnitude (a-b) < 1e-10 * magnitude(a+b)

instance (AlmostEq a, AlmostEq b) => AlmostEq (a, b) where
  (a1,b1) ~== (a2,b2) = a1 ~== a2 && b1 ~== b2

------------------------------------------------------------

data Angular = Deg Double | Cmp CN 
  deriving Show

instance AlmostEq Angular where  a ~== b = rad a ~== rad b

instance Eq Angular  where  a == b = a ~== b

instance Ord Angular where  a <= b = a == b || rad a < rad b

deg (Deg a) = Deg a
deg (Cmp v) | v ~== 0 = Deg 0
              | otherwise = Deg $ (180/pi * phase v) `mod'` 360

toCmp (Cmp x) = Cmp x
toCmp d = Cmp $ mkPolar 1 (rad d)

rad (Cmp v) = if v ~== 0 then 0 else phase v `mod'` (2*pi)
rad (Deg a) = (a*pi/180) `mod'` (2*pi)

toTurns = (/ (2*pi)) . rad

instance Num Angular where
  fromInteger n = Deg $ fromIntegral n `mod'` 360
  (+) = withAngular2 (+)
  (*) = withAngular2 (*)
  negate = withAngular negate
  abs = withAngular abs
  signum = withAngular signum

withAngular op a = let Deg a' = deg a
                   in Deg $ op a' `mod'` 360 
                  
withAngular2 op a b = let Deg a' = deg a
                          Deg b' = deg b
                      in Deg $ (a' `op` b') `mod'` 360 

------------------------------------------------------------

class Figure a where
  isTrivial :: a -> Bool
  isSimilar :: a -> a -> Bool

  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

------------------------------------------------------------

both f (a,b) = (f a, f b)
