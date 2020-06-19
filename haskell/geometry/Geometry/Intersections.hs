{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Geometry.Intersections
  ( -- ** Intersections of curves
    Intersections (..)
  , intersections, isIntersecting
  ) where

import Data.Complex

import Geometry.Base
import Geometry.Line
import Geometry.Circle
import Geometry.Polygon
import Geometry.Decorations

------------------------------------------------------------
-- | Class provides `intersections` function returning a list (possible empty)
-- of intersection points (co-dimension 1).
class (Manifold CN a, Manifold CN b) => Intersections a b where
  intersections' :: a -> b -> [CN]

intersections :: (Figure a, Figure b, Intersections a b)
  => a -> b -> [CN]
intersections a b  
  | isTrivial a = filter (isContaining b) [refPoint a]
  | isTrivial b = filter (isContaining a) [refPoint b]
  | otherwise = 
    filter (isContaining a) $
    filter (isContaining b) $
    intersections' a b

-- | Returns `True` if tho curves have intersection points.
isIntersecting :: (Figure a, Figure b, Intersections a b) => a -> b -> Bool
isIntersecting a b = not . null $ intersections a b

------------------------------------------------------------

intersectionCC c1 c2 | d == 0 || b < 0 = []
                     | b == 0          = [x]
                     | b > 0           = [x, conjugate x]
  where
    r1 = radius c1
    r2 = radius c2
    d = magnitude (center c1 - center c2)
    a = r1**2-r2**2+d**2
    b = -(r2-r1-d)*(r2-r1+d)*(r2+r1-d)*(r2+r1+d)
    x = scale (1/(2*d)) $ a :+ sqrt b

intersectionCL c l | b > r = []
                   | b == r = [0:+b]
                   | b < r = [a:+b, (-a):+b]
  where
    b = getY (refPoint l)
    r = radius c
    a = sqrt (r**2 - b**2)

------------------------------------------------------------

instance Intersections Line Line where
  intersections' l1 l2
    | l2 `isContaining` refPoint l1 = [refPoint l1]
    | l1 `isContaining` refPoint l2 = [refPoint l2]
    | otherwise = lineIntersection l1 l2

instance Intersections Line Circle  where
  intersections' l cir =
    invt <$> intersectionCL (t cir) (t l)
    where
      t :: Trans x => x -> x
      t = rotate (-a) . translate' (negate (center cir))
      invt = translate' (center cir) . rotate a
      a = angle l

instance Intersections Line Polyline where
  intersections' l = foldMap (intersections' l) . segments . closePolyline

------------------------------------------------------------

instance Intersections Circle Line where
  intersections' = flip intersections'

instance Intersections Circle Circle where
  intersections' cir1 cir2 =
    invt <$> intersectionCC (t cir1) (t cir2)
    where
      t = rotate (-a) . translate' (negate c1)
      invt = translate' c1 . rotate a
      c1 = center cir1
      c2 = center cir2
      a = azimuth c1 c2

instance Intersections Circle Polyline where
  intersections' c = foldMap (intersections' c) . segments . closePolyline

------------------------------------------------------------

instance Intersections Polyline Line where
  intersections' = flip intersections'

instance Intersections Polyline Circle where
  intersections' = flip intersections'
  
instance Intersections Polyline Polyline where
  intersections' p = foldMap (intersections' p) . segments . closePolyline

------------------------------------------------------------

deriving via Line instance
  (Manifold CN a, Intersections a Line) => Intersections a Ray

instance Intersections Ray Line where
  intersections' = intersections' . asLine 

instance Intersections Ray Polyline where
  intersections' = intersections' . asLine 

instance Intersections Ray Circle where
  intersections' = intersections' . asLine 

------------------------------------------------------------

deriving via Line instance
  (Manifold CN a, Intersections a Line) => Intersections a Segment

instance Intersections Segment Line where
  intersections' = intersections' . asLine 

instance Intersections Segment Polyline where
  intersections' = intersections' . asLine 

instance Intersections Segment Circle where
  intersections' = intersections' . asLine 

------------------------------------------------------------

deriving via Polyline instance
  (Manifold CN a, Intersections a Polyline) => Intersections a Polygon

instance Intersections Polygon Line  where
  intersections' = intersections' . asPolyline

instance Intersections Polygon Polyline  where
  intersections' = intersections' . asPolyline

instance Intersections Polygon Circle  where
  intersections' = intersections' . asPolyline

------------------------------------------------------------

deriving via Polygon instance
  (Manifold CN a, Intersections a Polyline) => Intersections a Triangle

instance Intersections Triangle Line  where
  intersections' = intersections' . asPolyline

instance Intersections Triangle Polyline  where
  intersections' = intersections' . asPolyline

instance Intersections Triangle Circle  where
  intersections' = intersections' . asPolyline

------------------------------------------------------------

deriving via Polygon instance
  (Manifold CN a, Intersections a Polyline) => Intersections a Rectangle

instance Intersections Rectangle Line  where
  intersections' = intersections' . asPolyline

instance Intersections Rectangle Polyline  where
  intersections' = intersections' . asPolyline

instance Intersections Rectangle Circle  where
  intersections' = intersections' . asPolyline