{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module SVG where

import Prelude hiding (writeFile)
import Graphics.Svg (doctype, svg11_, with, prettyText, (<<-))
import Graphics.Svg (Element, ToElement (..))
import Graphics.Svg.Elements
import Graphics.Svg.Attributes
import Data.Complex
import Data.Text hiding (center)
import Data.Text.Lazy.IO (writeFile)
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Affine
import Figure
import Point
import Circle
import Line
import Geometry

svgSize = 500

class SVGable a where
  fmtSVG :: a -> Text

instance SVGable Double where
  fmtSVG n = if n ~== 0 then "0" else toShortest n

instance SVGable CN where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y
    where (x, y) = coord p

instance SVGable XY where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y
    where (x, y) = coord p


instance ToElement Point where
  toElement p = let (x, y) = coord p
                in circle_ [ Cx_ <<- fmtSVG x
                             , Cy_ <<- fmtSVG y
                             , R_ <<- "3"
                             , Fill_ <<- "red"
                             , Stroke_ <<- "#444"
                             , Stroke_width_ <<- "1" ]

instance ToElement Circle where
  toElement c = circle_ [ Cx_ <<- fmtSVG x
                          , Cy_ <<- fmtSVG y
                          , R_ <<- fmtSVG (radius c)
                          , Fill_ <<- "none"
                          , Stroke_ <<- "orange"
                          , Stroke_width_ <<- "2" ]
    where (x :+ y) = center c

instance ToElement Line where
  toElement l = polyline_ [ Points_ <<- pts
                            , Fill_ <<- "none"
                            , Stroke_ <<- "orange"
                            , Stroke_width_ <<- "2" ]
    where
      pts = case l of
        Line _ -> fmtSVG (l <@ (-10)) <> " " <> fmtSVG (l <@ 10)
        Ray _ -> fmtSVG (l <@ 0) <> " " <> fmtSVG (l <@ 10)
        Segment _ -> fmtSVG (l <@ 0) <> " " <> fmtSVG (l <@ 1)


instance (Figure a, ToElement a) => ToElement (Labeled a) where
  toElement (Labeled ((l, c), f)) = toElement f <> text (toElement l)
    where
      text = text_ $ [ X_ <<- fmtSVG x
                     , Y_ <<- fmtSVG y
                     , Font_size_ <<- "16"
                     , Font_family_ <<- "CMU Serif"
                     , Font_style_ <<- "italic"
                     , Stroke_ <<- "none"
                     , Fill_ <<- "white"] <> offsetX <> offsetY 
      x :+ y = lp -- + scale lo lv
      lo = labelOffset f
      offsetX = case cornerX c of
                  -1 -> mempty
                  0 -> [ Text_anchor_ <<- "middle" ]
                  1 -> [ Text_anchor_ <<- "end" ]
      offsetY = case cornerY c of
                  -1 -> mempty
                  0 -> [Dy_ <<- "8"]
                  1 -> [Dy_ <<- "16"]
      
------------------------------------------------------------

data Group where 
    Nil :: Group
    Cons :: (ToElement a, Show a, Trans a) => a -> Group -> Group
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


instance ToElement Group where
    toElement Nil = mempty
    toElement (Cons x xs) = toElement x <> toElement xs
    toElement (Append x xs) = toElement x <> toElement xs

------------------------------------------------------------

svg content =
     doctype <>
     with (svg11_ content) [ Version_ <<- "1.1"
                           , Width_ <<- "500"
                           , Height_ <<- "500"
                           , Style_ <<- "background : #444;" ]

chart :: String -> Group -> IO ()
chart name figs = writeFile name $ prettyText contents
  where
    contents = svg $ toElement $ scaler figs
    scaler = translate ((svgSize/2) :+ (svgSize/2))
           . scale (svgSize/(paperSize + 1))

------------------------------------------------------------
