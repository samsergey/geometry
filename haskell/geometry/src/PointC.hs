{-# language DeriveFunctor,FlexibleInstances,UndecidableInstances #-}
module PointC where

import Data.Complex
import Data.Maybe

import Base

class PointC a where
  xy :: a -> CN
  mkPointC :: CN -> a

newtype Point' a = Point' a deriving Functor
newtype Label' a = Label' a deriving Functor

instance PointC a => Trans a where
  transform t p = mkPointC . (transform t y. xy

instance (Trans a, PointC a) => Affine a where
  cmp = xy
  fromCN = mkPointC
