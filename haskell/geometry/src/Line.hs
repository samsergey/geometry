{-# LANGUAGE OverloadedStrings #-}
module Line where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Generals
import Transform
import SVG

data Line = Line CXY CXY

instance Show Line where
  show (Line p1 p2) = concat ["<Line ", show (coord p1), ",", show (coord p2), ">"]


instance Eq Line where
  l1 == l2
    | isTrivial l1 = isTrivial l2 && l2 `isContaining` (l1 `param` 0)
    | otherwise = l2 `isContaining` (l1 `param` 0) &&
                  l2 `isContaining` (l1 `param` 1)


instance Figure Line where
  isTrivial l = vector l == 0
  isSimilar _ _ = True


instance Trans Line where
  transform t (Line p1 p2) = Line (transformCXY t p1) (transformCXY t p2)


instance Curve Line where
  param l t = start l + ((unit l * t) :+ 0) * vector l
  locus l p = let v = pos p - start l 
               in if isTrivial l then 0 else (v `dot` vector l) / unit l
  isClosed = const False
  isContaining l p = vector l `cross` (pos p - start l) == 0
  length _ = 1/0
  tangent l _ = Vec $ vector l


instance Linear Line where
  start (Line p _) = pos p
  end (Line _ p) = pos p
  vector (Line p1 p2) = normalize (p2 - p1)
  unit _ = 1


instance SVGable Line where
  toSVG c = Svg.polyline_ [ Svg.Points_ <<- pts
                          , Svg.Fill_ <<- "none"
                          , Svg.Stroke_ <<- "orange"
                          , Svg.Stroke_width_ <<- "2" ]
    where
      pts = fmtSVG (start c) <> " " <> fmtSVG (end c)

