{-# LANGUAGE DerivingVia #-}
module Line
  (-- * Types
    IsLine (..)
  , Line (..), trivialLine, mkLine, intersectionLL
  , Ray (..), mkRay
  , Segment (..), mkSegment, end
  ) where

import Data.Complex
import Data.List
import Data.Maybe
import Control.Monad

import Base

------------------------------------------------------------

bimap f (a, b) = (f a, f b)

-- | Class representing a linear object: line, ray or segment.
class (Affine l, Curve l, Trans l) => IsLine l where
  refPoints :: l -> (CN, CN)

  asLine :: l -> Line
  asLine = Line . refPoints

  asRay :: l -> Ray
  asRay = Ray . refPoints

  asSegment :: l -> Segment
  asSegment = Segment . refPoints

------------------------------------------------------------

-- | The straight line, passing through two given points.
-- The first point sets the `Figure`'s refference point and a starting point of a line.
-- The distance between refference points `p1` and `p2` sets the `unit` and internal scale,
-- so that `p1 == l \@<- 0` and `p2 == l \@<- 1`.
newtype Line = Line (CN, CN)
  deriving Show

instance IsLine Line where
  refPoints (Line r) = r

-- | The trivial line with coinsiding refference points.
trivialLine = Line (0,0)

-- | The basic line constructor.
mkLine ps = Line $ bimap cmp ps

instance Eq Line where
  l1 == l2 = refPoints l1 ~== refPoints l2

instance Figure Line where
  isTrivial = isZero

  refPoint = fst . refPoints

  box l = pointBox p1 <> pointBox p2
    where (p1,p2) = refPoints l
    

instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l 
           in cmp p2 - cmp p1
  asCmp p = Line (0, p)


instance Trans Line where
  transform t (Line ps) = Line $ bimap (transformCN t) ps


instance Manifold Line where
  isClosed _ = False

  bounds l | isTrivial l = [0, 0]
           | otherwise = []
   
  param l t = let (p1, p2) = refPoints l
              in scaleAt' p1 t p2

  project l p = let v = cmp p - cmp (refPoint l)
                in (v `dot` angle l) / unit l

  isContaining l p = cmp p ~== refPoint l
                     || l `isCollinear` azimuth (refPoint l) p

  unit = norm

instance Curve Line where

  isEnclosing _ _ = False

  orientation _ = 1

  tangent l _ = angle l

intersectionLL (x1 :+ y1) (v1x :+ v1y) (x2 :+ y2) (v2x :+ v2y) =
  [ (v1x*d2 - v2x*d1) :+ (v1y*d2 - v2y*d1) | d0 /= 0 ]
  where
    d0 = v1y*v2x - v1x*v2y
    d1 = (v1x*y1 - v1y*x1) / d0
    d2 = (v2x*y2 - v2y*x2) / d0

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
           , Curve
           , Figure
           ) via Line

-- | The basic ray constructor.
mkRay ps = Ray $ bimap cmp ps

instance IsLine Ray where
  refPoints (Ray r) = r

instance Manifold Ray where
  bounds r | isTrivial r = [0,0]
           | otherwise = [0]
  param = param . asLine
  project = project . asLine
  isContaining r p = isContaining (asLine r) p
                     && project r p >= 0
  unit = norm
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
           , Curve
           , Figure
           ) via Line

-- | The basic segment constructor.
mkSegment ps = Segment $ bimap cmp ps

instance IsLine Segment where
  refPoints (Segment r) = r

instance Manifold Segment where
  bounds s | isTrivial s = [0, 0]
           | otherwise = [0, 1]
  param = param . asLine
  project = project . asLine
  isContaining r p = isContaining (asRay r) p
                     && project r p <= 1      
  unit = norm

end :: Segment -> CN
end = snd . refPoints
