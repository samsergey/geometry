module Label where

import Data.Complex

import Base

data Label = Label String CN
  deriving Eq

labelText (Label s _) = s

instance Trans Label where
  transform t (Label s p) = Label s $ transformCN t p


instance Affine Label where
  cmp (Label _ p) = p
  fromCN = Label mempty


instance Show Label where
  show (Label s (x :+ y)) = concat ["<Label ", s, " (", sx, " ", sy, ")>"]
    where sx = show x
          sy = show y 

instance Figure Label where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = cmp
