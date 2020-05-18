{-# language MultiParamTypeClasses #-}
module Polygon where

import Data.Complex
import Data.Monoid

import Base
import Line

data Polygon = Polyline { vertices :: [CN] }
             | Polygon { vertices :: [CN] }


closePoly (Polyline p) = Polygon p
closePoly p = p

polyConstructor (Polyline _) = Polyline
polyConstructor (Polygon _) = Polygon

segments :: Polygon -> [Line]
segments p = Segment <$> zip vs (tail vs)
  where vs = case p of
          Polyline p -> p
          Polygon p -> p ++ [head p]

instance Show Polygon where
  show p = concat ["<", t, " ", n,">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"
          t = case p of
            Polyline _ -> "Polyline"
            Polygon _ -> "Polygon"

instance Eq Polygon where
  p1 == p2 = vertices p1 ~== vertices p2

instance Trans Polygon where
  transform t p = polyConstructor p $ transform t <$> vertices p

instance Curve Polygon where
  maybeParam p t = undefined
    where ds = scanl (+) 0 $ unit <$> segments p

  locus p pt = 0

  isClosed (Polyline _) = False
  isClosed (Polygon _) = True

  location pt p = res
    where res | any (`isContaining` pt) (segments p) = OnCurve
              | isClosed p && odd (length (intersections r p)) = Inside
              | otherwise   = Outside
          r = Ray (cmp pt, cmp pt + 1)

  unit p = sum $ zipWith distance vs (tail vs)
    where vs = vertices p

  tangent p t = undefined

instance Figure Polygon where
  isTrivial p = null $ vertices p
  isSimilar p1 p2 = p1 == p2
  refPoint p = head $ vertices p

instance Intersections Line Polygon where
  intersections = flip intersections
  
instance Intersections Polygon Line where
  intersections p l = foldMap (intersections l) (segments p)
