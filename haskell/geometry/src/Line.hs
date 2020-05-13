module Line where

import Data.Complex

import Base
import Affine

data Line = Line { start :: CN, through :: CN }
          | Segment { start :: CN, through :: CN }
          | Ray { start :: CN, through :: CN }

instance Show Line where
  show l = concat ["<", name, "("
                  , show x1, ",", show y1, "), ("
                  , show x2, ",", show y2, ")>"]
    where (x1, y1) = coord (start l)
          (x2, y2) = coord (through l)
          name = case l of
            Line _ _ -> "Line"
            Segment _ _ -> "Segment"
            Ray _ _ -> "Ray"

 
instance Eq Line where
  l1 == l2
    | isTrivial l1 = isTrivial l2 && l2 `isContaining` (l1 `param` 0)
    | otherwise = l2 `isContaining` (l1 `param` 0) &&
                  l2 `isContaining` (l1 `param` 1)


instance Figure Line where
  isTrivial l = vector l == 0
  isSimilar _ _ = True


instance Affine Line where
  cmp (Line p1 p2) = cmp p2 - cmp p1
  fromCN = Line 0


instance Linear Line where
  pivot (Line p _) = p


instance Trans Line where
  transform t (Line p1 p2) = Line (transformCN t p1) (transformCN t p2)


instance Curve Line where
  param l t = start l + ((unit l * t) :+ 0) * vector l
  locus l p = let v = cmp p - start l
               in if isTrivial l then 0 else (v `dot` vector l) / unit l
  isClosed = const False
  isContaining l p = vector l `collinear` (cmp p - start l)
  tangent l _ = Cmp $ vector l
  unit (Line p1 p2) = distance p1 p2




