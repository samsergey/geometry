{-# LANGUAGE OverloadedStrings #-}
module Point where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Generals
import Curve 
import Transform
import SVG

data Point = Point XY


instance Show Point where
  show (Point (x :+ y)) = "<Point " ++ show (x,y) ++ ">"


instance Trans Point where
  transform t (Point xy) = Point (transformXY t xy)


instance SVGable Point where
  toSVG (Point (x :+ y)) = Svg.circle_ [ Svg.Cx_ <<- fmtSVG x
                                       , Svg.Cy_ <<- fmtSVG y
                                       , Svg.R_ <<- "3"
                                       , Svg.Fill_ <<- "red"
                                       , Svg.Stroke_ <<- "#444"
                                       , Svg.Stroke_width_ <<- "1" ]
