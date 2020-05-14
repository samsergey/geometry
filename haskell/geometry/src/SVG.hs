{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module SVG where

import Graphics.Svg (doctype, svg11_, with, prettyText, (<<-))
import qualified Graphics.Svg as Svg
import qualified Graphics.Svg.Elements as E
import qualified Graphics.Svg.Attributes as A
import Data.Complex
import qualified Data.Text.Internal as Internal
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Txt
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Affine
import Figure
import Point
import Circle
import Line

paperSize = 50
svgSize = 500

class SVGable a where
  toSVG :: a -> Svg.Element
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
            in E.circle_ [ A.Cx_ <<- fmtSVG x
                         , A.Cy_ <<- fmtSVG y
                         , A.R_ <<- "3"
                         , A.Fill_ <<- "red"
                         , A.Stroke_ <<- "#444"
                         , A.Stroke_width_ <<- "1" ]

instance SVGable Circle where
  toSVG c = E.circle_ [ A.Cx_ <<- fmtSVG x
                      , A.Cy_ <<- fmtSVG y
                      , A.R_ <<- fmtSVG (radius c)
                      , A.Fill_ <<- "none"
                      , A.Stroke_ <<- "orange"
                      , A.Stroke_width_ <<- "2" ]
    where (x :+ y) = center c

instance SVGable Line where
  toSVG l = E.polyline_ [ A.Points_ <<- pts
                        , A.Fill_ <<- "none"
                        , A.Stroke_ <<- "orange"
                        , A.Stroke_width_ <<- "2" ]
    where
      pts = case l of
        Line _ -> fmtSVG (l <@ (-10)) <> " " <> fmtSVG (l <@ 10)
        Ray _ -> fmtSVG (l <@ 0) <> " " <> fmtSVG (l <@ 10)
        Segment _ -> fmtSVG (l <@ 0) <> " " <> fmtSVG (l <@ 1)

instance (Figure a, SVGable a) => SVGable (Labeled a) where
  toSVG (Labeled (l,f)) = toSVG f <> txt
    where
      txt = E.text_ [ A.X_ <<- fmtSVG x
                    , A.Y_ <<- fmtSVG y
                    , A.Font_size_ <<- "14"
                    , A.Text_anchor_ <<- "middle"] "dfg"
      x :+ y = lp + scale lo lv
      lp = cmp $ labelPosition f
      lo = labelOffset f
      lv = cmp $ labelOrientation f

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
     doctype <>
     with (svg11_ content) [ A.Version_ <<- "1.1"
                           , A.Width_ <<- "500"
                           , A.Height_ <<- "500" ]

chart :: String -> Group -> IO ()
chart name figs = Txt.writeFile name $ prettyText contents
  where
    contents = svg $ toSVG $ scaler figs
    scaler = translate ((svgSize/2) :+ (svgSize/2))
           . scale (svgSize/paperSize)
