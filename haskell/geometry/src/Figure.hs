module Figure where

import Base
import Affine

class Figure a where
  isTrivial :: a -> Bool
  isSimilar :: a -> a -> Bool

  isNontrivial :: a -> Bool
  isNontrivial x = not (isTrivial x)

  refPoint :: a -> CN
