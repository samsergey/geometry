{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Line
  (-- * Types
    Bounding (..)
  , Line (..)
  , IsLine (..)
  -- * Constructors
  , trivialLine, mkSegment, mkRay, mkLine
  -- * Functions
  , extendAs, clipBy
  ) where

import Data.Complex
import Data.List
import Data.Maybe

import Base

-- | Constrain for a linear object
class (Figure l, Affine l, Curve l, Trans l) => IsLine l where
  bounding :: l -> Bounding
  refPoints :: l -> (CN, CN)

-- | The type of line
data Bounding = Unbound    -- ^ a straight line
              | Semibound  -- ^ a ray
              | Bound      -- ^ a segment
  deriving (Eq, Show)

------------------------------------------------------------

-- | The straight line of a given type, passing through two given points.
-- The first point sets the `Figure`s' refference point and a starting point of a line.
-- The distance between refference points `p1` and `p2` sets the 'unit' and internal scale,
-- so that `p1 == l \@<- 0` and `p2 == l \@<- 1`.
data Line = Line Bounding !(CN, CN)

instance IsLine Line where
  bounding (Line b _) = b
  refPoints (Line _ r) = r

-- | The trivial line with coinsiding refference points.
trivialLine = mkLine (0,0)

-- | The basic line constructor 
mkLine = Line Unbound

-- | The basic ray constructor
mkRay = Line Semibound

-- | The basic segment constructor
mkSegment = Line Bound

instance Eq Line where
  l1 == l2 = bounding l1 == bounding l2 &&
             refPoints l1 ~== refPoints l2

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
l `extendAs` b =
  let res = Line b (refPoints l)
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
    where (x1, y1) = coord (l @-> 0)
          (x2, y2) = coord (l @-> 1)
          a = angle l

instance Figure Line where
  isTrivial l = cmp l == 0

  isSimilar l1 l2
    | bounding l1 /= bounding l2 = False
    | bounding l1 == Bound = unit l1 ~== unit l2
    | otherwise = True

  refPoint = fst . refPoints

  box l = case bounding l of
            _ -> pointBox p1 <> pointBox p2
--            Semibound -> ((pure (getX p1), mempty), (pure (getY p1), mempty))
--            Unbound -> mempty
    where (p1,p2) = refPoints l
    

instance Affine Line where
  cmp l =  let (p1, p2) = refPoints l in normalize $ cmp p2 - cmp p1
  asCmp p = mkLine (0, p)


instance Trans Line where
  transform t (Line b (p1,p2)) = Line b (p1', p2')
    where p1' = transformCN t p1
          p2' = transformCN t p2
  

instance Curve Line where
  unit l = let (p1, p2) = refPoints l in distance p1 p2

  param l t = let (p1, p2) = refPoints l in scaleAt' p1 t p2

  projectMaybe l p
    | isTrivial l = Nothing
    | otherwise =
        let p1 = fst $ refPoints l
            v = cmp p - cmp p1
            res = (v `dot` cmp l) / unit l
        in case bounding l of
             Unbound -> Just res
             Semibound -> if 0 ~<= res then Just res else Nothing
             Bound -> if 0 ~<= res && res ~<= 1 then Just res else Nothing

  isClosed _ = False
  isEnclosing _ _ = False
  orientation _ = 1

  isFinite l = case bounding l of
    Bound -> True
    _ -> False

  isContaining l p = let p1 = refPoint l
                         res = l `isCollinear` azimuth p1 p
                     in res && isJust (p ->@? l)

  tangent l _ = angle l

  distanceTo p l = case p ->@? l of
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

-- | Returns a list of segments as a result of clipping the line
-- by a closed curve.
clipBy :: (IsLine l, Intersections l c, Curve c) => l -> c -> [Line]
clipBy l c = filter internal $ Line Bound <$> zip ints (tail ints) 
  where
    ints = sortOn (project l) $ intersections l c <> ends
    internal s = c `isEnclosing` (s @-> 0.5)
    ends = case bounding l of
             Bound -> [l @-> 0, l @-> 1]
             Semibound -> [l @-> 0]
             Unbound -> []

