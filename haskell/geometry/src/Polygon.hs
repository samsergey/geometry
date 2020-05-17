module Polygon where

import Data.Complex

import Base

data Polygon = Polyline { vertices :: [CN] }
             | Polygon { vertices :: [CN] }

closePoly (Polyline p) = Polygon p
closePoly p = p

polyConstructor (Polyline _) = Polyline
polyConstructor (Polygon _) = Polygon

instance Show Polygon where
  show p = concat ["<", t , " (", vs, ")>"]
    where vs = unwords $ show . coord <$> vertices p
          t = case p of
            Polyline _ -> "Polyline"
            Polygon _ -> "Polygon"

instance Eq Polygon where
  p1 == p2 = vertices p1 ~== vertices p2

instance Trans Polygon where
  transform t p = polyConstructor p $ transform t <$> vertices p

instance Curve Polygon where
  param p t = head $ vertices p

  locus p pt = 0

  isClosed (Polyline _) = False
  isClosed (Polygon _) = True

  location p _ = undefined

  unit _ = 1

  tangent p t = undefined

instance Figure Polygon where
  isTrivial p = null $ vertices p
  isSimilar p1 p2 = p1 == p2
  refPoint p = head $ vertices p
