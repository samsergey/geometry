{-# language UndecidableInstances #-}
{-# language FlexibleInstances #-}
{-# language DeriveFunctor #-}
module PointC where

import Data.Complex
import Data.Maybe

import Base

------------------------------------------------------------

class PointC a where
  xy :: a -> CN
  mkPointC :: CN -> a

instance PointC a => Trans a where
  transform t p = mkPointC (transformCN t (xy p))

instance (Trans a, PointC a) => Affine a where
  cmp = xy
  fromCN = mkPointC

------------------------------------------------------------

newtype Point' = Point' CN 
newtype Label' = Label' CN 

instance PointC Point' where
  xy (Point' p) = p
  mkPointC = Point'

instance PointC Label' where
  xy (Label' p) = p
  mkPointC = Label'

instance Show Point' where
  show p = concat ["<Point (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

instance Show Label' where
  show p = concat ["<Label (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

------------------------------------------------------------

class CircleC a where
  radius :: a -> Double
  center :: a -> CN
  orientation :: a -> Double
  phaseShift :: a -> Double
  mkCircleC :: CN -> CN -> Double -> a

instance CircleC a => Trans a where
  transform t cir = mkCircle2 c p w
    where c = transformCN t (center cir)
          p = transformCN t (cir .@ 0)
          p' = transformCN t (cir .@ 0.25)
          w = signum $ cross (p - c) (p' - c)


------------------------------------------------------------

newtype Labeled a = Labeled (String, a)
  deriving Show

newtype Styled a = Styled (String, a)
  deriving Show


instance PointC a => PointC (Labeled a) where
  xy (Labeled (_, p)) = xy p
  mkPointC p = Labeled (mempty, mkPointC p)

instance PointC a => PointC (Styled a) where
  xy (Styled (_, p)) = xy p
  mkPointC p = Styled (mempty, mkPointC p)
