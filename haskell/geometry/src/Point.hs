module Point where

import Data.Complex
import Data.Bool

import Base

------------------------------------------------------------

newtype Point = Point CN

mkPoint :: Affine a => a -> Point
mkPoint = Point . cmp

instance Show Point where
  show p = concat ["<Point (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

instance Eq Point where
  p1 == p2 = cmp p1 ~== cmp p2

instance Trans Point where
  transform t (Point p) = Point $ transformCN t p 

instance Affine Point where
  cmp (Point p) = p
  asCmp = Point

instance Figure Point where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = cmp

------------------------------------------------------------

newtype Label = Label CN

mkLabel :: Affine a => a -> Label
mkLabel = Label . cmp

instance Show Label where
  show p = concat ["<Label (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

instance Eq Label where
  p1 == p2 = cmp p1 ~== cmp p2

instance Trans Label where
  transform t (Label p) = Label $ transformCN t p 

instance Affine Label where
  cmp (Label p) = p
  asCmp = Label

instance Figure Label where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = cmp

