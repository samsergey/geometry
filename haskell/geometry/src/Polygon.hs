{-# language MultiParamTypeClasses #-}
module Polygon where

import Data.Complex
import Data.Foldable
import Data.List.Extra
import Data.Monoid
import Data.Fixed (mod')

import Base
import Line

data Polygon = Polyline { polyOptions :: Options, vertices :: [CN] }
             | Polygon { polyOptions :: Options, vertices :: [CN] }

mkPolygon :: Affine a => [a] -> Polygon
mkPolygon pts = Polygon mempty $ cmp <$> pts
  
mkPolyline :: Affine a => [a] -> Polygon
mkPolyline pts = Polyline mempty $ cmp <$> pts

closePoly (Polyline o p) = Polygon o p
closePoly p = p

polyConstructor (Polyline _ _) = Polyline
polyConstructor (Polygon _ _) = Polygon

segments :: Polygon -> [Line]
segments p = mkSegment <$> zip vs (tail vs)
  where vs = case p of
          Polyline _ p -> p
          Polygon _ p -> p ++ [head p]

vertex :: Polygon -> Int -> CN
vertex (Polygon _ vs) i = vs !! (i `mod` length vs)
vertex (Polyline _ vs) i = vs !! ((0 `max` i) `min` length vs)

instance Show Polygon where
  show p = concat ["<", t, " ", n,">"]
    where vs = vertices p
          n = if length vs < 5
              then unwords $ show . coord <$> vs
              else "-" <> show (length vs) <> "-"
          t = case p of
            Polyline _ _ -> "Polyline"
            Polygon _ _ -> "Polygon"


instance Eq Polygon where
  p1 == p2 = vertices p1 ~== vertices p2


instance Trans Polygon where
  transform t p = polyConstructor p
                  (polyOptions p)
                  (transform t <$> vertices p)


instance Curve Polygon where
  maybeParam p t =
    case p of
      Polygon _ _ -> interpolation (t `mod'` unit p)
      Polyline _ _ -> interpolation t
    where
      interpolation x = param' <$> find interval tbl
        where
          interval ((a, b), _) = a ~<= x && x ~<= b
          param' ((a, b), s) = s .@ ((x - a)/(b-a))
          tbl = zip (zip ds (tail ds)) $ segments p
          ds = scanl (+) 0 $ unit <$> segments p

  locus p pt = x0 + pt @. s
    where
      ss = segments p
      ds = scanl (+) 0 $ unit <$> ss
      (x0, s) = minimumOn (\(_,s) -> s `distanceTo` pt) $ zip ds ss

  isClosed (Polyline _ _) = False
  isClosed (Polygon _ _) = True

  location pt p = res
    where res | any (`isContaining` pt) (segments p) = OnCurve
              | isClosed p && odd (length (intersections r p)) = Inside
              | otherwise   = Outside
          r = Ray mempty (cmp pt, cmp pt + 1)

  unit p = sum $ unit <$> segments p

  tangent p t = undefined

  distanceTo p pt = minimum $ (`distanceTo` pt) <$> segments p
  

instance Figure Polygon where
  options = polyOptions
  setOptions o p = p { polyOptions = polyOptions p <> o }

  isTrivial p = null $ vertices p

  isSimilar p1 p2 = p1 == p2

  refPoint p = if isNontrivial p
               then head $ vertices p
               else 0

  labelDefaults = mempty

instance Intersections Line Polygon where
  intersections = flip intersections

instance Intersections Polygon Line where
  intersections p l = foldMap (intersections l) (segments p)
