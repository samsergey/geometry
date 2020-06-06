{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingVia #-}

module Polygon
  (
    IsPolyline (..)
  , Polyline (..)
  , mkPolyline, trivialPolyline, closePoly
  , Polygon (..)
  , mkPolygon, trivialPolygon
  , Triangle (..)
  , mkTriangle, trivialTriangle
  , Rectangle (..)
  , mkRectangle, trivialRectangle
  , boxRectangle
  )
where

import Data.Complex
import Data.Foldable
import Data.List.Extra (minimumOn)
import Data.Monoid
import Data.Fixed

import Base
import Point
import Line

class Curve p => IsPolyline p where
  vertices :: p -> [CN]
  asPolyline :: p -> Polyline
  
  verticesNumber :: p -> Int
  verticesNumber p = length (vertices p)

  segments :: p -> [Segment]
  segments p = mkSegment <$> zip (vertices p) (tail vs)
    where vs = if isClosed p
               then cycle (vertices p)
               else vertices p
                    
  vertex :: p -> Int -> CN
  vertex p i = vs !! j
    where vs = vertices p
          n = verticesNumber p
          j = if isClosed p
              then i `mod` n
              else (0 `max` i) `min` n
                   
  side :: p -> Int -> Segment
  side p i = segments p !! j
    where j = if isClosed p
              then (i + n `div` 2) `mod` n
              else (0 `max` i) `min` n
          n = verticesNumber p
  

interpolation :: IsPolyline p => p -> Double -> Maybe CN
interpolation p x = param' <$> find interval tbl
  where
    interval ((a, b), _) = a ~<= x && x ~<= b
    param' ((a, b), s) = s @-> ((x - a)/(b-a))
    tbl = zip (zip ds (tail ds)) $ segments p
    ds = scanl (+) 0 $ unit <$> segments p

------------------------------------------------------------

newtype Polyline = Polyline [CN]

instance IsPolyline Polyline where
  vertices (Polyline vs) = vs
  asPolyline = id
  
trivialPolyline :: Polyline
trivialPolyline = Polyline []

mkPolyline :: Affine a => [a] -> Polyline
mkPolyline pts = Polyline $ cmp <$> pts

closePoly :: Polyline -> Polygon
closePoly p = Polygon (vertices p)

instance Show Polyline where
  show p = concat ["<Polyline ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"

instance Eq Polyline where
  p1 == p2 = vertices p1 ~== vertices p2

instance Affine Polyline where
  cmp p = case segments p of
            [] -> 0
            (x:_) -> cmp x
  asCmp x = Polyline [0, x]

instance Trans Polyline where
  transform t (Polyline vs) = Polyline $ transform t <$> vs

instance Manifold Polyline where
  paramMaybe p t = interpolation p $ t * unit p

  projectMaybe p pt = if (0 <= x && x <= 1)
                      then Just x
                      else Nothing
    where
      ss = segments p
      ds = scanl (+) 0 $ unit <$> ss
      (x0, s) = minimumOn (\(_,s) -> distanceTo pt s) $ zip ds ss
      x = (x0 + projectL s pt) / unit p

  isClosed _ = False
  isContaining p x = any (`isContaining` x) (segments p)
  unit p = sum $ unit <$> segments p

instance Curve Polyline where
  orientation _ = 1

  location p pt = res
    where res | any (`isContaining` pt) (segments p) = OnCurve
              | otherwise = Outside

  tangent p t =  (p @-> (t + dt)) `azimuth` (p @-> (t - dt))
    where dt = 1e-5

 
instance Figure Polyline where
  isTrivial p = length (vertices p) < 2
  isSimilar p1 p2 = p1 == p2
  refPoint p = if isNontrivial p
               then head $ vertices p
               else 0
  box p = foldMap (box . mkPoint) (vertices p)


instance Intersections Line Polyline where
  intersections' l p = foldMap (intersections l) (segments p)

instance Intersections Ray Polyline where
  intersections' l p = foldMap (intersections l) (segments p)

instance Intersections Segment Polyline where
  intersections' l p = foldMap (intersections l) (segments p)

instance {-# OVERLAPPING #-}
  Intersections Polyline Polyline where
  intersections' p1 p2 = foldMap (intersections p1) (segments p2)

instance (Curve a, Intersections a Polyline) =>
         Intersections Polyline a where
  intersections' = flip intersections'

------------------------------------------------------------

newtype Polygon = Polygon [CN]
  deriving ( Eq
           , Trans
           , Affine
           , Figure
           ) via Polyline

trivialPolygon :: Polygon
trivialPolygon = Polygon []

mkPolygon :: Affine a => [a] -> Polygon
mkPolygon pts = Polygon $ cmp <$> pts

instance IsPolyline Polygon where
  vertices (Polygon vs) = vs
  asPolyline (Polygon vs) = Polyline vs

instance Show Polygon where
  show p = concat ["<Polygon ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"

instance Manifold Polygon where
  paramMaybe p t = interpolation p $ (t `mod'` 1) * unit p
  projectMaybe = projectMaybe . asPolyline
  isClosed _ = True
  isContaining p x = any (`isContaining` x) (segments p)
  unit p = sum $ unit <$> segments p
  
instance Curve Polygon where
  orientation _ = 1
  location p pt = res
    where res | any (`isContaining` pt) (segments p) = OnCurve
              | isClosed p && odd (length (intersections p r)) = Inside
              | otherwise = Outside
          r = mkRay (cmp pt, cmp pt + 1)

  tangent = tangent . asPolyline


instance (Curve a, Intersections a Polyline) => Intersections a Polygon where
  intersections' x t = intersections' x (asPolyline t)

instance (Curve b, Intersections Polyline b) => Intersections Polygon b where
  intersections' t = intersections' (asPolyline t)


------------------------------------------------------------

newtype Triangle = Triangle [CN]
  deriving ( Figure
           , Manifold
           , Curve
           , Trans
           , Eq
           , IsPolyline
           ) via Polygon

mkTriangle :: Affine a => [a] -> Triangle
mkTriangle = Triangle . fmap cmp

trivialTriangle = Triangle []

instance Show Triangle where
  show t = concat ["<Triangle ", ss, ">"]
    where ss = unwords $ show . coord <$> vertices t

instance Affine Triangle where
  cmp = cmp . asPolyline
  asCmp x = Triangle [0, x, rotate 60 x]

instance (Curve a, Intersections a Polyline) => Intersections a Triangle where
  intersections' x t = intersections' x (asPolyline t)

instance (Curve b, Intersections Polyline b) => Intersections Triangle b where
  intersections' t = intersections' (asPolyline t)

------------------------------------------------------------

newtype Rectangle = Rectangle [CN]
  deriving ( Figure
           , Manifold
           , Curve
           , Trans
           , Eq
           , IsPolyline
           ) via Polygon

mkRectangle :: Affine a => [a] -> Rectangle
mkRectangle = Rectangle . fmap cmp

trivialRectangle = Rectangle []

instance Show Rectangle where
  show t = concat ["<Rectangle ", ss, ">"]
    where ss = unwords $ show . coord <$> vertices t

instance Affine Rectangle where
  cmp = cmp . asPolyline
  asCmp (x:+y) = Rectangle [0, x:+0, x:+y, 0:+y]

instance (Curve a, Intersections a Polyline) => Intersections a Rectangle where
  intersections' x t = intersections' x (asPolyline t)

instance (Curve b, Intersections Polyline b) => Intersections Rectangle b where
  intersections' t = intersections' (asPolyline t)

boxRectangle f = mkRectangle [ p4, p3, p2, p1 ]
  where ((p4,p3),(p1,p2)) = corner f
