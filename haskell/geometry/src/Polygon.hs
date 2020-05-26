{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Polygon where

import Data.Complex
import Data.Foldable
import Data.List.Extra
import Data.Monoid
import Data.Fixed

import Base
import Line

class Curve p => Polygonal p where
  vertices :: p -> [CN]
  polyClosed :: p -> Bool

data Polygon = Polygon Bool ![CN]

instance Polygonal Polygon where
  vertices (Polygon _ vs) = vs
  polyClosed (Polygon c _) = c
  

mkPolygon :: Affine a => [a] -> Polygon
mkPolygon pts = Polygon True $ cmp <$> pts
  
mkPolyline :: Affine a => [a] -> Polygon
mkPolyline pts = Polygon False $ cmp <$> pts

closePoly :: Polygon -> Polygon
closePoly p = Polygon True (vertices p)

segments :: Polygonal p => p -> [Line]
segments p = mkSegment <$> zip (vertices p) (tail vs)
  where vs = if isClosed p
             then cycle (vertices p)
             else vertices p

vertex :: Polygonal p => p -> Int -> CN
vertex p i = vs !! j
  where vs = vertices p
        j = if isClosed p
            then i `mod` length vs
            else (0 `max` i) `min` length vs


instance Show Polygon where
  show p = concat ["<", t, " ", n,">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"
          t = if isClosed p then "Polygon" else "Polyline"


instance Eq Polygon where
  p1 == p2 = isClosed p1 == isClosed p2 &&
             vertices p1 ~== vertices p2


instance Trans Polygon where
  transform t (Polygon c vs) = Polygon c $ transform t <$> vs


instance Curve Polygon where
  paramMaybe p t = if isClosed p
                   then interpolation (t `mod'` unit p)
                   else interpolation t
    where
      interpolation x = param' <$> find interval tbl
        where
          interval ((a, b), _) = a ~<= x && x ~<= b
          param' ((a, b), s) = s @-> ((x - a)/(b-a))
          tbl = zip (zip ds (tail ds)) $ segments p
          ds = scanl (+) 0 $ unit <$> segments p

  project p pt = x0 + pt ->@ s
    where
      ss = segments p
      ds = scanl (+) 0 $ unit <$> ss
      (x0, s) = minimumOn (\(_,s) -> s `distanceTo` pt) $ zip ds ss

  isClosed = polyClosed

  location pt p = res
    where res | any (`isContaining` pt) (segments p) = OnCurve
              | isClosed p && odd (length (intersections r p)) = Inside
              | otherwise   = Outside
          r = mkRay (cmp pt, cmp pt + 1)

  unit p = sum $ unit <$> segments p

  tangent p t = undefined

  distanceTo p pt = minimum $ (`distanceTo` pt) <$> segments p
  
instance Figure Polygon where
  isTrivial p = null $ vertices p
  isSimilar p1 p2 = p1 == p2
  refPoint p = if isNontrivial p
               then head $ vertices p
               else 0

instance Intersections Line Polygon where
  intersections = flip intersections

instance Intersections Polygon Line where
  intersections p l = foldMap (intersections l) (segments p)
