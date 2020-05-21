{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
module Line where

import Data.Complex
import Data.List

import Base

data Bounding = Unbound | Semibound | Bound
  deriving (Eq, Show)

data Line = Line { bounding :: Bounding
                 , refPoints :: !(CN, CN) }

trivialLine = mkLine (0,0)
mkLine = Line Unbound
mkRay = Line Semibound
mkSegment = Line Bound

instance Eq Line where
  l1 == l2 = bounding l1 == bounding l2 &&
             refPoints l1 ~== refPoints l2

l `extendAs` b =
  let res = l { bounding = b }
  in case bounding l of
       Bound -> res
       Semibound -> case b of
         Bound -> l
         _ -> res
       Unbound -> l


instance Show Line where
  show l = case bounding l of
    Bound -> unwords [ "<Segment"
                         , "(" <> show x1 <> "," <> show y1 <> "),"
                         , "(" <> show x2 <> "," <> show y2 <> ")>"]
    Unbound -> unwords [ "<Line"
                      , "(" <> show x1 <> "," <> show y1 <> "),"
                      , show a <> ">"]
    Semibound -> unwords [ "<Ray"
                     , "(" <> show x1 <> "," <> show y1 <> "),"
                     , show a <> ">"]
    where (x1, y1) = coord (l .@ 0)
          (x2, y2) = coord (l .@ 1)
          a = angle l

instance Figure Line where
  isTrivial l = cmp l == 0

  isSimilar l1 l2
    | bounding l1 /= bounding l2 = False
    | bounding l1 == Bound = unit l1 ~== unit l2
    | otherwise = True

  refPoint = fst . refPoints

instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l in normalize $ cmp p2 - cmp p1
  fromCN p = mkLine (0, p)


instance Trans Line where
  transform t l = l { refPoints = (p1, p2) }
    where p1 = transformCN t $ l .@ 0
          p2 = transformCN t $ l .@ 1
  

instance Curve Line where
  unit l = let (p1, p2) = refPoints l in distance p1 p2

  param l t = let (p1, p2) = refPoints l in scaleAt p1 t p2

  locus l p | isTrivial l = 0
            | otherwise = let p1 = fst $ refPoints l
                              v = cmp p - cmp p1
                          in (v `dot` cmp l) / unit l

  isClosed _ = False
  isEnclosing _ _ = False

  isContaining l p = case bounding l of
    Unbound   -> res
    Semibound -> cmp p ~== start l || (res && 0 <= x)
    Bound     -> cmp p ~== start l || (res && 0 <= x && x ~<= 1)
    where p1 = refPoint l
          x = p @. l
          res = l `isCollinear` azimuth p1 p

  tangent l _ = angle l

  distanceTo l p = case p ?. l of
    Just x -> p `distance` (l .@ x)
    Nothing -> (p `distance` (l .@ 0)) `min`
               (p `distance` (l .@ 1))


instance Intersections Line Line where
  intersections l1 l2
    | isTrivial l1 = filter (isContaining l2) [refPoint l1]
    | isTrivial l2 = filter (isContaining l1) [refPoint l2]
    | otherwise = 
      filter (isContaining l1) $
      filter (isContaining l2) $
      intersectionV (refPoint l1) (cmp l1) (refPoint l2) (cmp l2)


intersectionV (x1 :+ y1) (v1x :+ v1y) (x2 :+ y2) (v2x :+ v2y) =
  [(v1x*d2 - v2x*d1) :+ (v1y*d2 - v2y*d1) | d0 /= 0]
  where
    d0 = v1y*v2x - v1x*v2y
    d1 = (v1x*y1 - v1y*x1) / d0
    d2 = (v2x*y2 - v2y*x2) / d0


clipBy :: (Intersections Line c, Curve c) => Line -> c -> [Line]
clipBy l c = filter internal $ Line Bound <$> zip ints (tail ints) 
  where
    ints = sortOn (locus l) $ intersections l c <> ends
    internal s = c `isEnclosing` (s .@ 1/2)
    ends = case bounding l of
             Bound -> [l .@ 0, l .@ 1]
             Semibound -> [l .@ 0]
             Unbound -> []

