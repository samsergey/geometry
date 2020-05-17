{-# Language MultiParamTypeClasses #-}

module Line where

import Data.Complex

import Base

data Line = Line {refPoints :: (CN, CN) }
          | Segment {refPoints :: (CN, CN) }
          | Ray {refPoints :: (CN, CN) }

lineConstructor (Line _) = Line
lineConstructor (Ray _) = Ray
lineConstructor (Segment _) = Segment

extendAs :: Line -> ((CN, CN) -> Line) -> Line
l1 `extendAs` l = let res = l (refPoints l1)
                   in case l1 of
                     Segment _ -> res
                     Ray _ -> case res of
                                Segment _ -> l1
                                _ -> res
                     Line _  -> l1


instance Show Line where
  show l = case l of
    Segment _ -> unwords [ "<Segment"
                         , "(" <> show x1 <> "," <> show y1 <> "),"
                         , "(" <> show x2 <> "," <> show y2 <> ")>"]
    Line _ -> unwords [ "<Line"
                      , "(" <> show x1 <> "," <> show y1 <> "),"
                      , show a <> ">"]
    Ray _ -> unwords [ "<Ray"
                     , "(" <> show x1 <> "," <> show y1 <> "),"
                     , show a <> ">"]
    where (x1, y1) = coord (l <@ 0)
          (x2, y2) = coord (l <@ 1)
          a = angle l

 
instance Eq Line where
  l1 == l2
    | isTrivial l1 = isTrivial l2 && l2 `isContaining` (l1 <@ 0)
    | otherwise = l2 `isContaining` (l1 <@ 0) &&
                  l2 `isContaining` (l1 <@ 1)


instance Figure Line where
  isTrivial l = cmp l == 0
  isSimilar s1@(Segment _) s2@(Segment _) = unit s1 ~== unit s2
  isSimilar l1 l2 = True
  refPoint = fst . refPoints
  labelPosition l = case l of
    Segment _ -> l <@ 0.5
    _ -> l <@ 1
  labelOffset l = coord $ scale 12 $ normal l 0
  labelCorner _ = (0,0)


instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l in normalize $ cmp p2 - cmp p1
  fromCN p = Line (0, p)


instance Trans Line where
  transform t l = lineConstructor l (transformCN t p1, transformCN t p2)
    where p1 = l <@ 0
          p2 = l <@ 1
  

instance Curve Line where
  unit l = let (p1, p2) = refPoints l in distance p1 p2

  param l t = let (p1, p2) = refPoints l in scaleAt p1 t p2

  locus l p | isTrivial l = 0
            | otherwise = let p1 = fst $ refPoints l
                              v = cmp p - cmp p1
                          in (v `dot` cmp l) / unit l

  isClosed = const False

  isContaining l p = case l of
    Line _    -> res
    Ray _     -> res && 0 ~<= x
    Segment _ -> res && 0 ~<= x && x ~<= 1
    where (p1, _) = refPoints l
          x = p @> l
          res = angle l `isCollinear` azimuth p1 (cmp p)

  tangent l _ = angle l

intersectionV p1 v1 p2 v2 =
  [(det (x12, d), det (y12, d)) | d0 /= 0]
  where
    (x12, y12) = transpose (v1, v2)
    d0 = det (v1, v2)
    d = (det (v1, p1) / d0,  det (p2, v2) / d0)

instance Intersections Line Line where
  intersections l1 l2
    | isTrivial l1 = filter (isContaining l2) [coord $ refPoint l1]
    | isTrivial l2 = filter (isContaining l1) [coord $ refPoint l2]
    | otherwise = 
      filter (isContaining l1) $
      filter (isContaining l2) $
      intersectionV (refPoint l1) l1 (refPoint l2) l2


