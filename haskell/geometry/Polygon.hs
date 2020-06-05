{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}

module Polygon
  (
    IsPolygon (..)
  , Polyline (..)
  , mkPolyline, trivialPolyline, closePolyline
  , Polygon (..)
  , mkPolygon, trivialPolygon
  , Triangle
  , mkTriangle, trivialTriangle
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

class Curve p => IsPolygon p where
  vertices :: p -> [CN]

  verticesNumber :: p -> Int
  verticesNumber p = length (vertices p)

  segments :: p -> [Line]
  segments p = mkSegment <$> zip (vertices p) (tail vs)
    where vs = if isClosed p
               then cycle (vertices p)
               else vertices p

  vertex :: p -> Int -> CN
  vertex p i = vs !! j
    where vs = vertices p
          j = if isClosed p
              then i `mod` length vs
              else (0 `max` i) `min` length vs

  side :: p -> Int -> Line
  side p i = segments p !! j
    where j = if isClosed p
              then (i + n `div` 2) `mod` n
              else (0 `max` i) `min` n
          n = length (segments p)

interpolation :: IsPolygon p => p -> Double -> Maybe CN
interpolation p x = param' <$> find interval tbl
  where
    interval ((a, b), _) = a ~<= x && x ~<= b
    param' ((a, b), s) = s @-> ((x - a)/(b-a))
    tbl = zip (zip ds (tail ds)) $ segments p
    ds = scanl (+) 0 $ unit <$> segments p

------------------------------------------------------------

data Polyline = Polyline ![CN]

instance IsPolygon Polyline where
  vertices (Polyline vs) = vs
  
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
  asCmp x = mkPolyline [0, x]

instance Trans Polyline where
  transform t (Polyline vs) = Polyline $ transform t <$> vs

instance Curve Polyline where
  paramMaybe p t = interpolation p $ t * unit p

  project p pt = (x0 + projectL s pt) / unit p
    where
      ss = segments p
      ds = scanl (+) 0 $ unit <$> ss
      (x0, s) = minimumOn (\(_,s) -> distanceTo pt s) $ zip ds ss

  isClosed _ = False

  orientation _ = 1

  location pt p = res
    where res | any (`isContaining` pt) (segments p) = OnCurve
              | isClosed p && odd (length (intersections r p)) = Inside
              | otherwise = Outside
          r = mkRay (cmp pt, cmp pt + 1)

  unit p = sum $ unit <$> segments p

  tangent p t =  (p @-> (t + dt)) `azimuth` (p @-> (t - dt))
    where dt = 1e-5

  distanceTo pt p = minimum $ distanceTo pt <$> segments p

 
instance Figure Polyline where
  isTrivial p = length (vertices p) < 2
  isSimilar p1 p2 = p1 == p2
  refPoint p = if isNontrivial p
               then head $ vertices p
               else 0
  box p = foldMap (box . mkPoint) (vertices p)


instance Intersections Line Polyline where
  intersections = flip intersections

instance Intersections Polyline Line where
  intersections p l = foldMap (intersections l) (segments p)

instance Intersections Polyline Polyline where
  intersections p1 p2 = foldMap (intersections p1) (segments p2)

------------------------------------------------------------

data Polygon = Polygon ![CN]

instance IsPolygon Polygon where
  vertices (Polygon vs) = vs
  
trivialPolygon :: Polygon
trivialPolygon = Polygon []

mkPolygon :: Affine a => [a] -> Polygon
mkPolygon pts = Polygon $ cmp <$> pts

asPolyline :: Polygon -> Polyline
asPolyline (Polygon []) = Polyline []
asPolyline (Polygon vs) = Polyline $ vs ++ head vs
  
instance Show Polygon where
  show p = concat ["<Polygon ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"

instance Eq Polygon where
  p1 == p2 = vertices p1 ~== vertices p2

instance Affine Polygon where
  cmp p = case segments p of
            [] -> 0
            (x:_) -> cmp x
  asCmp x = mkPolygon [0, x]

instance Trans Polygon where
  transform t (Polygon vs) = Polygon $ transform t <$> vs

instance Curve Polygon where
  paramMaybe p t = interpolation p $ (t `mod'` 1) * unit p

  project p = project (asPolyline p)

  isClosed _ = True 

  orientation _ = 1

  location pt p = res
    where res | any (`isContaining` pt) (segments p) = OnCurve
              | isClosed p && odd (length (intersections r p)) = Inside
              | otherwise = Outside
          r = mkRay (cmp pt, cmp pt + 1)

  unit p = sum $ unit <$> segments p

  tangent p t =  (p @-> (t + dt)) `azimuth` (p @-> (t - dt))
    where dt = 1e-5

  distanceTo pt p = minimum $ distanceTo pt <$> segments p

 
instance Figure Polygon where
  isTrivial p = length (vertices p) < 2
  isSimilar p1 p2 = p1 == p2
  refPoint p = if isNontrivial p
               then head $ vertices p
               else 0
  box p = foldMap (box . mkPoint) (vertices p)

instance Intersections Line Polygon where
  intersections = flip intersections

instance Intersections Polygon Line where
  intersections p l = foldMap (intersections l) (segments p)

instance Intersections Polygon Polygon where
  intersections p1 p2 = foldMap (intersections p1) (segments p2)

instance Intersections Polygon Polyline where
  intersections = flip intersections

instance Intersections Polyline Polygon where
  intersections p l = foldMap (intersections l) (segments p)

------------------------------------------------------------

boxRectangle f = mkPolygon [ p4, p3, p2, p1 ]
  where ((p4,p3),(p1,p2)) = corner f

------------------------------------------------------------

newtype Triangle = Triangle { fromTriangle :: Polygon }
  deriving (Figure, Curve, Trans, Eq, Show)

mkTriangle vs = Triangle $ mkPolygon vs

trivialTriangle = Triangle trivialPolygon

instance IsPolygon Triangle where
  verticesNumber _ = 3

  vertices = vertices . fromTriangle

  segments p = mkSegment <$> zip (vertices p) (tail vs)
    where vs = cycle (vertices p)

  vertex p i = vertices p !! (i `mod` 3)

instance Affine Triangle where
  cmp = cmp . fromTriangle
  asCmp x = mkTriangle [0, x, rotate 60 x]

instance (Curve a, Intersections a Polygon) => Intersections a Triangle where
  intersections x t = intersections x (fromTriangle t)

instance (Curve b, Intersections Polygon b) => Intersections Triangle b where
  intersections t = intersections (fromTriangle t)
