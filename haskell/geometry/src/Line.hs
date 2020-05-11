{-# LANGUAGE OverloadedStrings #-}
module Line where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Generals
import Curve 
import Transform
import SVG

data Line = Line XY XY

instance Show Line where
  show (Line p1 p2) = concat ["<Line ", show (x1, y1), ",", show (x2, y2), ">"]
    where (x1 :+ y1) = p1
          (x2 :+ y2) = p2


instance Trans Line where
  transform t (Line p1 p2) = Line (transformXY t p1) (transformXY t p2)


instance Curve Line where
  param l t = start l + ((unit l * t) :+ 0) * vector l
  locus l xy = 0
  closed = const False
  length _ = 1/0
  tangent l _ = Vec $ vector l


instance Linear Line where
  start (Line p _) = p
  end (Line _ p) = p
  vector (Line p1 p2) = normalize (p2 - p1)
  unit _ = 1

instance SVGable Line where
  toSVG c = Svg.polyline_ [ Svg.Points_ <<- pts
                          , Svg.Fill_ <<- "none"
                          , Svg.Stroke_ <<- "orange"
                          , Svg.Stroke_width_ <<- "2" ]
    where
      pts = fmtSVG (start c) <> " " <> fmtSVG (end c)

