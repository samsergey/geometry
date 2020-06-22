{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Geometry.Line
  ( Linear (..)
  , Line (..), trivialLine, mkLine, lineIntersection
  , Ray (..), mkRay
  , Segment (..), mkSegment, end, midPerpendicular
  ) where

import Data.Complex
import Data.List
import Data.Maybe
import Control.Monad

import Geometry.Base

------------------------------------------------------------

-- | Class representing a linear object: line, ray or segment.
class (Curve CN l, Affine l, Figure l) => Linear l where
  {-# MINIMAL refPoints #-}
  refPoints :: l -> (CN, CN)

  asLine :: l -> Line
  asLine = Line . refPoints

  asRay :: l -> Ray
  asRay = Ray . refPoints

  asSegment :: l -> Segment
  asSegment = Segment . refPoints

instance Linear l => Linear (Maybe l) where
  refPoints = maybe (0,0) refPoints
  
------------------------------------------------------------

-- | The straight line, passing through two given points.
-- The first point sets the `Figure`'s refference point and a starting point of a line.
-- The distance between refference points `p1` and `p2` sets the `unit` and internal scale,
-- so that `p1 == l \@<- 0` and `p2 == l \@<- 1`.
newtype Line = Line (CN, CN)
  deriving Show

instance Linear Line where
  refPoints (Line ps) = ps

-- | The trivial line with coinsiding refference points.
trivialLine = Line (0, 0)

-- | The basic line constructor.
mkLine :: (Affine p1, Affine p2) => (p1, p2) -> Line
mkLine (p1, p2) = Line (cmp p1, cmp p2)

instance Eq Line where
  l1 == l2 = refPoints l1 ~== refPoints l2

instance Figure Line where
  isTrivial = isZero
  refPoint = fst . refPoints
  box l = pointBox (p1 - p2) <> pointBox p2
    where (p1, p2) = refPoints l
   

instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l 
           in cmp p2 - cmp p1
  asCmp p = mkLine (0 :: CN, p)


instance Trans Line where
  transform t (Line (p1, p2)) = Line (p1', p2')
    where p1' = transformCN t p1
          p2' = transformCN t p2


instance Manifold CN Line where
  bounds l | isTrivial l = [0, 0]
           | otherwise = []
  param l t = let (p1, p2) = refPoints l
              in scaleAt' p1 t p2
  project l p = let v = cmp p - cmp (refPoint l)
                in (v `dot` angle l) / unit l
  isContaining l p = cmp p ~== refPoint l
                     || l `isCollinear` azimuth (refPoint l) p
  unit = norm

instance Curve CN Line where
  tangent l _ = angle l

intersectionLL (x1 :+ y1) (v1x :+ v1y) (x2 :+ y2) (v2x :+ v2y) =
  [ (v1x*d2 - v2x*d1) :+ (v1y*d2 - v2y*d1) | d0 /= 0 ]
  where
    d0 = v1y*v2x - v1x*v2y
    d1 = (v1x*y1 - v1y*x1) / d0
    d2 = (v2x*y2 - v2y*x2) / d0

lineIntersection l1 l2 =
  intersectionLL (refPoint l1) (cmp l1) (refPoint l2) (cmp l2)

------------------------------------------------------------

-- | The ray, passing through two given points.
-- The first point sets the `Figure`'s refference point and a starting point of a ray.
-- The distance between refference points `p1` and `p2` sets the `unit` and internal scale,
-- so that `p1 == l \@<- 0` and `p2 == l \@<- 1`.
newtype Ray = Ray (CN, CN)
  deriving Show
  deriving ( Eq
           , Affine
           , Trans
           , Curve CN
           , Linear
           ) via Line

-- | The basic ray constructor.
mkRay :: (Affine p1, Affine p2) => (p1, p2) -> Ray
mkRay = asRay . mkLine


instance Manifold CN Ray where
  bounds r | isTrivial r = [0,0]
           | otherwise = [0]
  param = param . asLine
  project = project . asLine
  isContaining r p = isContaining (asLine r) p
                     && project r p >= 0
  unit = norm

instance Figure Ray where
  isTrivial = isZero
  refPoint = fst . refPoints
  box l = pointBox p1 <> pointBox p2
    where (p1, p2) = refPoints l

------------------------------------------------------------

-- | The line segment, joining two given points.
-- The first point sets the `Figure`'s refference point and a starting point of a ray.
-- The distance between refference points `p1` and `p2` sets the `unit` and internal scale,
-- so that `p1 == l \@<- 0` and `p2 == l \@<- 1`.
newtype Segment = Segment (CN, CN)
  deriving Show
  deriving ( Eq
           , Affine
           , Trans
           , Curve CN
           , Linear
           ) via Line

-- | The basic segment constructor.
mkSegment :: (Affine p1, Affine p2) => (p1, p2) -> Segment
mkSegment = asSegment . mkLine

instance Manifold CN Segment where
  bounds s | isTrivial s = [0, 0]
           | otherwise = [0, 1]
  param = param . asLine
  project = project . asLine
  isContaining r p = isContaining (asRay r) p
                     && project r p <= 1      
  unit = norm

instance Figure Segment where
  isTrivial = isZero
  refPoint = fst . refPoints
  box l = pointBox p1 <> pointBox p2
    where (p1, p2) = refPoints l


end :: Segment -> CN
end = snd . refPoints

midPerpendicular :: Segment -> Line
midPerpendicular s = mkLine (p1, p2)
  where p1 = s @-> 0.5
        p2 = p1 + cmp (normal s 0.5)
