{-# LANGUAGE OverloadedStrings #-}
module Geometry where

import Graphics.Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Generals
import Curve
import Transform
import SVG

------------------------------------------------------------
data Figure = P Point
            | C Circle
            | Group [Figure]
  deriving Show

instance Trans Figure where
  transform t fig = case fig of
    Group fs -> Group $ transform t <$> fs
    P p -> P $ transform t p
    C c -> C $ transform t c

instance SVGable Figure where
  toSVG fig = case fig of
    Group fs -> foldMap toSVG fs
    P p -> toSVG p
    C c -> toSVG c

------------------------------------------------------------

data Point = Point XY deriving Show

pt (x, y) = P $ Point (x :+ y)

instance Trans Point where
  transform t (Point xy) = Point (transformXY t xy)

instance SVGable Point where
  toSVG (Point (x :+ y)) = circle_ [ Cx_ <<- toPrecision 8 x
                                   , Cy_ <<- toPrecision 8 y
                                   , R_ <<- "3"
                                   , Fill_ <<- "red"
                                   , Stroke_ <<- "#444"
                                   , Stroke_width_ <<- "1" ]

------------------------------------------------------------

data Circle = Circle { radius :: Number
                     , center :: XY
                     , orientation :: Number
                     , phaseShift :: Number }
            deriving Show

circle r (x,y) = C $ mkCircle c (c + (r :+ 0))
  where c = x :+ y

mkCircle c p = Circle (magnitude r) c 1 (phase r)
  where r = p - c

instance Trans Circle where
  transform t cir = mkCircle c p
    where c = transformXY t (center cir)
          p = transformXY t (cir `point` 0)

instance Curve Circle where
  point cir t = 0

instance SVGable Circle where
  toSVG c = circle_ [ Cx_ <<- toPrecision 8 x
                    , Cy_ <<- toPrecision 8 y
                    , R_ <<- toPrecision 8 (radius c)
                    , Fill_ <<- "none"
                    , Stroke_ <<- "orange"
                    , Stroke_width_ <<- "2" ]
    where (x :+ y) = center c

------------------------------------------------------------

