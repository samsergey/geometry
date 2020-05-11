{-# LANGUAGE OverloadedStrings #-}
module Circle where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Generals
import Curve 
import Transform
import SVG

data Circle = Circle { radius :: Number
                     , center :: XY
                     , orientation :: Number
                     , phaseShift :: Number }


instance Show Circle where
  show cir = concat ["<Circle ", show r, ",", show (x,y), ">"]
    where r = radius cir
          (x :+ y) = center cir


instance Trans Circle where
  transform t cir = mkCircle c p
    where c = transformXY t (center cir)
          p = transformXY t (cir `param` 0)


instance Curve Circle where
  param cir t = center cir + mkPolar (radius cir) (2*pi*t)
  locus cir xy = phase (xy - center cir) / (2*pi)
  closed = const True
  length cir = radius cir * 2* pi
  tangent cir t = 90 + Vec (cir `param` t - center cir)
  

instance SVGable Circle where
  toSVG c = Svg.circle_ [ Svg.Cx_ <<- fmtSVG x
                        , Svg.Cy_ <<- fmtSVG y
                        , Svg.R_ <<- fmtSVG (radius c)
                        , Svg.Fill_ <<- "none"
                        , Svg.Stroke_ <<- "orange"
                        , Svg.Stroke_width_ <<- "2" ]
    where (x :+ y) = center c

mkCircle c p = Circle (magnitude r) c 1 (phase r)
  where r = p - c
