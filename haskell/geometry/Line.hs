{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}

module Line
  (-- * Types
    IsLine (..)
  , Line (..)
  , trivialLine, mkLine
  -- * Functions
--  , extendAs, clipBy
  ) where

import Data.Complex
import Data.List
import Data.Maybe

import Base

bimap f (a, b) = (f a, f b)

-- | Class representing a linear object.
class (Figure l, Affine l, Curve l, Trans l) => IsLine l where
  bounding :: l -> Double -> Bool
  refPoints :: l -> (CN, CN)

------------------------------------------------------------

-- | The straight line, passing through two given points.
-- The first point sets the `Figure`s' refference point and a starting point of a line.
-- The distance between refference points `p1` and `p2` sets the 'unit' and internal scale,
-- so that `p1 == l \@<- 0` and `p2 == l \@<- 1`.
newtype Line = Line (CN, CN)

instance IsLine Line where
  bounding _ _ = True
  refPoints (Line r) = r

-- | The trivial line with coinsiding refference points.
trivialLine = Line (0,0)

-- | The basic line constructor.
mkLine ps = Line $ bimap cmp ps

instance Eq Line where
  l1 == l2 = refPoints l1 ~== refPoints l2

instance Show Line where
  show l = unwords [ "<Line", show p, show a <> ">" ]
    where p = coord (l @-> 0)
          a = angle l

instance Figure Line where
  isTrivial l = cmp l == 0

  refPoint = fst . refPoints

  box l = pointBox p1 <> pointBox p2
    where (p1,p2) = refPoints l
    

instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l 
           in normalize $ cmp p2 - cmp p1
  asCmp p = mkLine (0, p)


instance Trans Line where
  transform t (Line ps) = Line $ bimap (transformCN t) ps
  

instance Curve Line where
  unit l = let (p1, p2) = refPoints l
           in distance p1 p2

  paramMaybe l t = let (p1, p2) = refPoints l
                   in if bounding l t
                      then Just $ scaleAt' p1 t p2
                      else Nothing

  projectMaybe l p
    | isTrivial l = Nothing
    | otherwise =
        let (p1, _) = refPoints l
            v = cmp p - cmp p1
            t = (v `dot` cmp l) / unit l
        in if bounding l t
           then Just t
           else Nothing

  isClosed _ = False
  isEnclosing _ _ = False
  orientation _ = 1

  isFinite _ = False

  isContaining l p = let p1 = refPoint l
                         res = l `isCollinear` azimuth p1 p
                     in res && isJust (projectMaybe l p)

  tangent l _ = angle l

  distanceTo p l = case projectMaybe l p of
    Just x -> p `distance` (l @-> x)
    Nothing -> (p `distance` (l @-> 0)) `min`
               (p `distance` (l @-> 1))


instance Intersections Line Line where
  intersections l1 l2
    | l1 `isCollinear` l2 = []
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


------------------------------------------------------------

newtype Ray = Ray (CN, CN)
  deriving (Eq, Affine, Trans, Curve, Figure) via Line

-- | The basic ray constructor.
mkRay ps = Ray $ bimap cmp ps

instance IsLine Ray where
  bounding _ x = x >= 0
  refPoints (Ray r) = r

instance Show Ray where
  show l = unwords [ "<Ray", show p, show a <> ">" ]
    where p = coord (l @-> 0)
          a = angle l

------------------------------------------------------------

newtype Segment = Segment (CN, CN)
  deriving (Eq, Affine, Trans, Curve, Figure) via Line

-- | The basic segment constructor.
mkSegment ps = Segment $ bimap cmp ps


instance IsLine Segment where
  bounding _ x = x >= 0 && x <= 1
  refPoints (Segment r) = r

instance Show Segment where
  show l = unwords [ "<Segment", show p1 <> ",", show p2, ">" ]
    where p1 = coord (l @-> 0)
          p2 = coord (l @-> 1)


-- | Returns a list of segments as a result of clipping the line
-- by a closed curve.

-- clipBy :: (IsLine l, Intersections l c, Curve c) => l -> c -> [Line]
-- clipBy l c = filter internal $ Line Bound <$> zip ints (tail ints) 
--   where
--     ints = sortOn (project l) $ intersections l c <> ends
--     internal s = c `isEnclosing` (s @-> 0.5)
--     ends = case bounding l of
--              Bound -> [l @-> 0, l @-> 1]
--              Semibound -> [l @-> 0]
--              Unbound -> []

-- | The extension of a line with the same refference points.
--
-- >>> mkSegment (0, 1) `extendAs` Semibound
-- <Ray (0.0,0.0), 0.0°>
--
-- >>> mkSegment (0, 1) `extendAs` Unbound
-- <Line (0.0,0.0), 0.0°>
--
-- >>> mkRay (0, 1) `extendAs` Unbound
-- <Line (0.0,0.0), 0.0°>
-- 


-- l `extendAs` b =
--   let res = Line b (refPoints l)
--   in case bounding l of
--        Bound -> res
--        Semibound -> case b of
--          Bound -> l
--          _ -> res
--        Unbound -> l
