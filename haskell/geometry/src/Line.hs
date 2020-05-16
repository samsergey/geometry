{-# Language MultiParamTypeClasses #-}

module Line where

import Data.Complex

import Base
import Affine
import Figure

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

instance Intersections Line Line where
  intersections l1 l2 = []

intersectionV (x1, y1) (v1x, v1y) (x2, y2) (v2x, v2y) = [res | det == 0]
  where  
    det = v1x*v2y - v1y*v2x
    det1 = (v1x*y1 - v1y*x1)/det
    det2 = (v2y*x2 - v2x*y2)/det
    res = (v1x*det2 + v2x*det1, v1y*det2 + v2y*det1)

intersectionV2 :: Affine a => (a, a) -> (a, a) -> [a]
intersectionV2 (p1, v1) (p2, v2) = [res | d0 /= 0]
  where  
    d0 = det (v1, v2)
    d = fromCoord (det (v1, p1), det (v2, p2))
    (vx, vy) = tr (v1, v2)
    res = fromCoord (det (d, vx) / d0, det (d, vy) / d0)
