module Line where

import Data.Complex

import Base
import Affine

data Line = Line {refPoints :: (CN, CN) }
          | Segment {refPoints :: (CN, CN) }
          | Ray {refPoints :: (CN, CN) }

lineConstructor (Line _) = Line
lineConstructor (Ray _) = Ray
lineConstructor (Segment _) = Segment

instance Show Line where
  show l = case l of
    Segment _ -> unwords ["<Segment"
                         , "(" <> show x1 <> "," <> show y1 <> "),"
                         , "(" <> show x2 <> "," <> show y2 <> ")>"]
    Line _ -> unwords ["<Line"
                      , "(" <> show x1 <> "," <> show y1 <> "),"
                      , show a <> ">"]
    Ray _ -> unwords ["<Ray"
                     , "(" <> show x1 <> "," <> show y1 <> "),"
                     , show a <> ">"]
    where (x1, y1) = coord (l `param` 0)
          (x2, y2) = coord (l `param` 1)
          a = angle l

 
instance Eq Line where
  l1 == l2
    | isTrivial l1 = isTrivial l2 && l2 `isContaining` (l1 `param` 0)
    | otherwise = l2 `isContaining` (l1 `param` 0) &&
                  l2 `isContaining` (l1 `param` 1)


instance Figure Line where
  isTrivial l = cmp l == 0
  isSimilar s1@(Segment _) s2@(Segment _) = unit s1 == unit s2
  isSimilar l1 l2 = True

instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l in normalize $ cmp p2 - cmp p1
  fromCN p = Line (0, p)

instance Trans Line where
  transform t l = lineConstructor l (transformCN t p1, transformCN t p2)
    where p1 = l `param` 0
          p2 = l `param` 1
  

instance Curve Line where
  unit l = let (p1, p2) = refPoints l in distance p1 p2

  param l t = let (p1, p2) = refPoints l in scaleAt p1 t p2

  locus l p | isTrivial l = 0
            | otherwise = let p1 = fst $ refPoints l
                              v = cmp p - cmp p1
                          in (v `dot` cmp l) / unit l

  isClosed = const False

  isContaining l p = let (p1, _) = refPoints l
                     in angle l `collinear` azimuth p1 (cmp p)

  tangent l _ = angle l

