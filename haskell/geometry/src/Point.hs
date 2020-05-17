module Point where

import Data.Complex

import Base

newtype Point = Point XY


instance Eq Point where
  p1 == p2 = cmp p1 ~== cmp p2


instance Trans Point where
  transform t (Point p) = Point $ transformXY t p


instance Affine Point where
  coord (Point p) = p
  fromCoord = Point


instance Show Point where
  show (Point (x, y)) = concat ["<Point (", sx, " ", sy, ")>"]
    where sx = show x
          sy = show y 

instance Figure Point where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = cmp
