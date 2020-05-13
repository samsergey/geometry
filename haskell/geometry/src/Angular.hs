{-#Lnaguage FlexibleInstances #-}
module Angular where

import Data.Fixed (mod')
import Data.Complex

class Angular a where
  {-# MINIMAL rad | deg #-}
  deg :: a -> Double
  deg x = rad x / pi * 180 `mod'` 360
  
  rad :: a -> Double
  rad x = deg x * pi / 180 `mod'` (2*pi)

  cmp :: a -> Complex Double
  cmp x = mkPolar 1 (rad x)
  
  turns :: a -> Double
  turns x = rad x / (2*pi) `mod'` 1


newtype Deg a = Deg a deriving Show
newtype Rad a = Rad a deriving Show
newtype Cmp a = Cmp a deriving Show
newtype Turns a = Turns a deriving Show

instance Angular (Deg a) where
  deg (Deg d) = d `mod'` 360

-- instance Angular (Rad Double) where
--   rad (Rad r) = r `mod'` (2*pi)

-- instance Angular (Turns Double) where
--   rad (Turns t) = t * 2 * pi

-- instance Angular (Cmp (Complex Double)) where
--   deg (Cmp x) = phase x


--instance Angular a => AlmostEq a  where
--  a ~== b = rad a ~== rad b


-- instance Eq Angular  where
--   a == b = a ~== b

-- instance Ord Angular where
--   a <= b = a == b || toRad a < toRad b


-- toDeg (Deg a) = Deg a
-- toDeg (Rad a) = Deg $ ((a / pi * 180) `mod'` 360)
-- toDeg (Turns a) = Deg $ ((a * 360) `mod'` 360)
-- toDeg (Cmp v) | v ~== 0 = Deg 0
--               | otherwise = Deg $ (180/pi * phase v) `mod'` 360

-- toCmp (Deg a) = Cmp $ mkPolar 1 (a/180*pi)
-- toCmp (Rad x) = Cmp x
-- toCmp (Cmp x) = Cmp x
-- toCmp x = toCmp $ toRad x


-- toRad (Cmp v) = if v ~== 0 then 0 else phase v `mod'` (2*pi)
-- toRad (Deg a) = (a*pi/180) `mod'` (2*pi)

-- toTurns = (/ (2*pi)) . toRad

-- instance Num Angular where
--   fromInteger n = Deg $ fromIntegral n `mod'` 360
--   (+) = withAngular2 (+)
--   (*) = withAngular2 (*)
--   negate = withAngular negate
--   abs = withAngular abs
--   signum = withAngular signum

-- withAngular op a = let Deg a' = toDeg a
--                in Deg $ op a' `mod'` 360 
                  
-- withAngular2 op a b = let Deg a' = toDeg a
--                       Deg b' = toDeg b
--                   in Deg $ (a' `op` b') `mod'` 360 

