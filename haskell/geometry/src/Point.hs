{-# LANGUAGE OverloadedStrings #-}
module Point where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex
import Data.AEq
import Test.QuickCheck

import Base
import Transform
import SVG


data Point = Point CXY

instance Eq Point where
  p1 == p2 = pos p1 ~== pos p2

instance Show Point where
  show p = "<Point " ++ show (fmtSVG (coord p)) ++ ">"


instance Trans Point where
  transform t (Point p) = Point $ transformCXY t p


instance Pos Point where
  pos (Point p) = p


instance SVGable Point where
  toSVG p = let (x, y) = coord p
            in Svg.circle_ [ Svg.Cx_ <<- fmtSVG x
                           , Svg.Cy_ <<- fmtSVG y
                           , Svg.R_ <<- "3"
                           , Svg.Fill_ <<- "red"
                           , Svg.Stroke_ <<- "#444"
                           , Svg.Stroke_width_ <<- "1" ]


instance Arbitrary Point where
  arbitrary = Point <$> arbitrary
