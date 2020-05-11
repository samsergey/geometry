{-# LANGUAGE OverloadedStrings #-}
module Point where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex

import Generals
import Transform
import SVG


data Point = Point CXY


instance Show Point where
  show p = "<Point " ++ show (coord p) ++ ">"


instance Trans Point where
  transform t (Point p) = Point $ transformCXY t p


instance Pos Point where
  pos (Point p) = p


instance SVGable Point where
  toSVG (Point (x :+ y)) = Svg.circle_ [ Svg.Cx_ <<- fmtSVG x
                                       , Svg.Cy_ <<- fmtSVG y
                                       , Svg.R_ <<- "3"
                                       , Svg.Fill_ <<- "red"
                                       , Svg.Stroke_ <<- "#444"
                                       , Svg.Stroke_width_ <<- "1" ]
