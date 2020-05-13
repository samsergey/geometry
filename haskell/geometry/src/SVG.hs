{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module SVG where

import qualified Graphics.Svg as Svg
import Graphics.Svg (Element, prettyText, (<<-))
import Data.Complex
import qualified Data.Text.Internal as Internal
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Txt
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Affine
import Point
import Circle
import Line

paperSize = 50
svgSize = 500

class SVGable a where
  toSVG :: a -> Element
  toSVG x = mempty

  fmtSVG :: a -> Internal.Text
  fmtSVG = mempty

instance SVGable Double where
  fmtSVG n = if n ~== 0 then "0" else toShortest n

instance SVGable CN where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y
    where (x, y) = coord p

instance SVGable XY where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y
    where (x, y) = coord p

instance SVGable Point where
  toSVG p = let (x, y) = coord p
            in Svg.circle_ [ Svg.Cx_ <<- fmtSVG x
                           , Svg.Cy_ <<- fmtSVG y
                           , Svg.R_ <<- "3"
                           , Svg.Fill_ <<- "red"
                           , Svg.Stroke_ <<- "#444"
                           , Svg.Stroke_width_ <<- "1" ]

instance SVGable Circle where
  toSVG c = Svg.circle_ [ Svg.Cx_ <<- fmtSVG x
                        , Svg.Cy_ <<- fmtSVG y
                        , Svg.R_ <<- fmtSVG (radius c)
                        , Svg.Fill_ <<- "none"
                        , Svg.Stroke_ <<- "orange"
                        , Svg.Stroke_width_ <<- "2" ]
    where (x :+ y) = center c

instance SVGable Line where
  toSVG l = Svg.polyline_ [ Svg.Points_ <<- pts
                          , Svg.Fill_ <<- "none"
                          , Svg.Stroke_ <<- "orange"
                          , Svg.Stroke_width_ <<- "2" ]
    where
      pts = fmtSVG (start l) <> " " <> fmtSVG (end l)

------------------------------------------------------------

data Group where 
    Nil :: Group
    Cons :: (SVGable a, Show a, Trans a) => a -> Group -> Group
    Append :: Group -> Group -> Group

infixr 5 <+>
a <+> b = Append (Cons a Nil) (Cons b Nil)

instance Trans Group where
    transform t Nil = Nil
    transform t (Cons x xs) = Cons (transform t x) (transform t xs)
    transform t (Append x xs) = Append (transform t x) (transform t xs)

instance Show Group where
    show Nil = mempty
    show (Cons x xs) = show x <> show xs
    show (Append x xs) = show x <> show xs


instance SVGable Group where
    toSVG Nil = mempty
    toSVG (Cons x xs) = toSVG x <> toSVG xs
    toSVG (Append x xs) = toSVG x <> toSVG xs

------------------------------------------------------------

svg content =
     Svg.doctype <>
     Svg.with (Svg.svg11_ content) [ Svg.Version_ <<- "1.1"
                                   , Svg.Width_ <<- "500"
                                   , Svg.Height_ <<- "500" ]

chart :: String -> Group -> IO ()
chart name figs = Txt.writeFile name $ prettyText contents
  where
    contents = svg $ toSVG $ scaler figs
    scaler = translate ((svgSize/2) :+ (svgSize/2))
           . scale (svgSize/paperSize)
