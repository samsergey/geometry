{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
module Line where

import Data.Complex
import Data.List

import Base

data Line = Line { lineOptions :: Options , refPoints :: (CN, CN) }
          | Segment { lineOptions :: Options , refPoints :: (CN, CN) }
          | Ray { lineOptions :: Options , refPoints :: (CN, CN) }

lineType (Line _ _) = "Line"
lineType (Ray _ _) = "Ray"
lineType (Segment _ _) = "Segment"

lineConstructor (Line _ _) = Line
lineConstructor (Ray _ _)  = Ray
lineConstructor (Segment _ _) = Segment

trivialLine = mkLine (0,0)
mkLine ps = Line mempty ps
mkRay ps = Ray mempty ps
mkSegment ps = Segment mempty ps

instance Eq Line where
  l1 == l2 = lineType l1 == lineType l2 &&
             refPoints l1 ~== refPoints l2

l1 `extendAs` l = let res = l (lineOptions l1) (refPoints l1)
                   in case lineType l1 of
                     "Segment" -> res
                     "Ray" -> case lineType res of
                       "Segment" -> l1
                       _ -> res
                     "Line" -> l1


instance Show Line where
  show l = case lineType l of
    "Segment" -> unwords [ "<Segment"
                         , "(" <> show x1 <> "," <> show y1 <> "),"
                         , "(" <> show x2 <> "," <> show y2 <> ")>"]
    "Line" -> unwords [ "<Line"
                      , "(" <> show x1 <> "," <> show y1 <> "),"
                      , show a <> ">"]
    "Ray" -> unwords [ "<Ray"
                     , "(" <> show x1 <> "," <> show y1 <> "),"
                     , show a <> ">"]
    where (x1, y1) = coord (l .@ 0)
          (x2, y2) = coord (l .@ 1)
          a = angle l

instance Figure Line where
  options = lineOptions
  setOptions o p = p { lineOptions = lineOptions p <> o }

  labelDefaults l =
    LabelSettings { getLabel = mempty
                  , getLabelPosition = pure $ l .@ 0.5
                  , getLabelOffset = pure $ coord $ scale 1 $ normal l 0
                  , getLabelCorner = pure (0,0)
                  , getLabelAngle = pure 0 }

  isTrivial l = cmp l == 0

  isSimilar l1 l2
    | lineType l1 /= lineType l2 = False
    | lineType l1 == "Segment" = unit l1 ~== unit l2
    | otherwise = True

  refPoint = fst . refPoints

instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l in normalize $ cmp p2 - cmp p1
  fromCN p = Line mempty (0, p)


instance Trans Line where
  transform t l = constr (transformCN t p1, transformCN t p2)
    where p1 = l .@ 0
          p2 = l .@ 1
          constr ps = lineConstructor l (lineOptions l) ps
  

instance Curve Line where
  unit l = let (p1, p2) = refPoints l in distance p1 p2

  param l t = let (p1, p2) = refPoints l in scaleAt p1 t p2

  locus l p | isTrivial l = 0
            | otherwise = let p1 = fst $ refPoints l
                              v = cmp p - cmp p1
                          in (v `dot` cmp l) / unit l

  isClosed = const False

  isContaining l p = case lineType l of
    "Line"    -> res
    "Ray"     -> cmp p ~== start l || (res && 0 <= x)
    "Segment" -> cmp p ~== start l || (res && 0 <= x && x ~<= 1)
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
clipBy l c = filter internal $ Segment opts <$> zip ints (tail ints) 
  where
    opts = lineOptions l
    ints = sortOn (locus l) $ intersections l c <> ends
    internal s = c `isEnclosing` (s .@ 1/2)
    ends = case lineType l of
             "Segment" -> [l .@ 0, l .@ 1]
             "Ray" -> [l .@ 0]
             "Line" -> []

