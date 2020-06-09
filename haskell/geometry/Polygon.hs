{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language DerivingVia #-}
{-# language TupleSections #-}

module Polygon
  (
    IsPolyline (..)
  , isDegenerate
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
import Data.Maybe
import Data.Fixed

import Base
import Point
import Line

class Oriented p => IsPolyline p where
  vertices :: p -> [CN]
  asPolyline :: p -> Polyline
  
  verticesNumber :: p -> Int
  verticesNumber p = length (vertices p)

  segments :: p -> [Segment]
  segments p = (Segment . (, o)) <$> zip vs (tail vs)
    where vs = vertices p
          o = orientation p

  vertex :: p -> Int -> CN
  vertex p i = vs !! j
    where vs = vertices p
          n = verticesNumber p
          j = (0 `max` i) `min` n

  side :: p -> Int -> Segment
  side p i = segments p !! j
    where j = (0 `max` i) `min` n
          n = verticesNumber p         
 

isDegenerate :: IsPolyline p => p -> Bool
isDegenerate = any isZero . segments 

interpolation :: IsPolyline p => p -> Double -> Maybe CN
interpolation p x = param' <$> find interval tbl
  where
    interval ((a, b), _) = a ~<= x && x ~<= b
    param' ((a, b), s) = s @-> ((x - a)/(b-a))
    tbl = zip (zip ds (tail ds)) $ segments p
    ds = scanl (+) 0 $ unit <$> segments p

------------------------------------------------------------

newtype Polyline = Polyline ([CN], Bool)

instance IsPolyline Polyline where
  vertices (Polyline (vs, _)) = vs
  asPolyline = id
  
trivialPolyline :: Polyline
trivialPolyline = Polyline ([], True)

mkPolyline :: Affine a => [a] -> Polyline
mkPolyline pts = Polyline (cmp <$> pts, True)

closePoly :: Polyline -> Polygon
closePoly p = Polygon (vertices p, orientation p)


instance Show Polyline where
  show p = concat ["<Polyline ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"


instance Eq Polyline where
  p1 == p2 = vertices p1 ~== vertices p2 &&
             orientation p1 == orientation p2


instance Affine Polyline where
  cmp p = case segments p of
            [] -> 0
            (x:_) -> cmp x
  asCmp x = mkPolyline [0, x]


instance Trans Polyline where
  transform t (Polyline (vs, o)) = Polyline (vs', o)
    where vs' = transform t <$> vs


instance Manifold Polyline where
  param p t | t < 0 = param (asLine (side p 0)) t
            | t > 1 = param (asLine (side p (verticesNumber p - 1))) t
            | otherwise = fromJust $  interpolation p (t * unit p) 

  project p pt = (x0 + (project s pt * unit s)) / unit p
    where
      ss = segments p
      ds = scanl (+) 0 $ unit <$> ss
      (x0, s) = minimumOn (\(_,s) -> distanceTo pt s) $ zip ds ss
      x = (x0 + (project s pt * unit s)) / unit p

  isContaining p x = any (`isContaining` x) (segments p)
  unit p = sum $ unit <$> segments p


instance Oriented Polyline where
  orientation (Polyline (_, o)) = o
  setOrientation o (Polyline (ps, _)) = Polyline (ps, not o)
  tangent p t =  (p @-> (t + dt)) `azimuth` (p @-> (t - dt))
    where dt = 1e-5

 
instance Figure Polyline where
  isTrivial p = length (vertices p) < 2
  isSimilar p1 p2 = p1 == p2
  refPoint p = if isNontrivial p
               then head $ vertices p
               else 0
  box p = foldMap (box . mkPoint) (vertices p)


------------------------------------------------------------

newtype Polygon = Polygon ([CN], Bool)
  deriving ( Eq
           , Trans
           , Affine
           , Oriented
           , Figure
           ) via Polyline

trivialPolygon :: Polygon
trivialPolygon = Polygon ([], True)

mkPolygon :: Affine a => [a] -> Polygon
mkPolygon pts = Polygon (cmp <$> pts, True)


instance IsPolyline Polygon where
  asPolyline p = Polyline (take (length vs + 1) (cycle vs), o)
    where vs = vertices p
          o = orientation p
  vertices = vertices . asPolyline


instance Show Polygon where
  show p = concat ["<Polygon ", n, ">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"

instance Manifold Polygon where
  param p t = fromJust $ interpolation p $ (t `mod'` 1) * unit p
  project = project . asPolyline
  isContaining = isContaining . asPolyline
  unit = unit . asPolyline

  
instance ClosedCurve Polygon where
  location p pt = case foldMap go (segments p') of
                    (Any True, _) -> OnCurve
                    (_, Sum n) | odd n -> Inside
                    _ -> Outside
    where
      p' = p # translate' (negate (cmp pt))
      go s | y0 * y1 == 0       = (Any True, mempty)
           | y0 == y1           = mempty
           | x == 0             = (Any True, mempty)
           | x > 0 && y0*y1 < 0 = (mempty, Sum 1)
           | otherwise          = mempty
        where
          (x0:+y0, x1:+y1) = refPoints s 
          x = (x0*y1-x1*y0)/(y1-y0)       

------------------------------------------------------------

newtype Triangle = Triangle ([CN], Bool)
  deriving ( Figure
           , Manifold
           , Oriented
           , ClosedCurve
           , Trans
           , Eq
           , IsPolyline
           ) via Polygon

mkTriangle :: Affine a => [a] -> Triangle
mkTriangle ps = Triangle (cmp <$> ps, True)

trivialTriangle = Triangle ([], True)

instance Show Triangle where
  show t = concat ["<Triangle ", ss, ">"]
    where ss = unwords $ show . coord <$> vertices t

instance Affine Triangle where
  cmp = cmp . asPolyline
  asCmp = mkTriangle . scanl (+) 0 . take 2 . iterate (rotate 120)

------------------------------------------------------------

newtype Rectangle = Rectangle ([CN], Bool)
  deriving ( Figure
           , Manifold
           , Oriented
           , ClosedCurve
           , Trans
           , Eq
           , IsPolyline
           ) via Polygon

mkRectangle :: Affine a => [a] -> Rectangle
mkRectangle ps = Rectangle (cmp <$> ps, True)

trivialRectangle = Rectangle ([], False)

instance Show Rectangle where
  show t = concat ["<Rectangle ", ss, ">"]
    where ss = unwords $ show . coord <$> vertices t

instance Affine Rectangle where
  cmp = cmp . asPolyline
  asCmp = mkRectangle . scanl (+) 0 . take 3 . iterate (rotate 90)


boxRectangle f = mkRectangle [ p4, p3, p2, p1 ]
  where ((p4,p3),(p1,p2)) = corner f
