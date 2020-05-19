module Point where

import Data.Complex

import Base

data Point = Point LabelData CN

instance Eq Point where
  p1 == p2 = cmp p1 ~== cmp p2


instance Trans Point where
  transform t (Point l p) = Point l $ transformCN t p


instance Affine Point where
  cmp (Point _ p) = p
  fromCN = Point mempty


instance Show Point where
  show p = concat ["<Point ", sl, " (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p
          sl = getLabel p

instance Figure Point where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = cmp
  labelData (Point l _) = l
  appLabelData l (Point l' p) = Point (l <> l') p
