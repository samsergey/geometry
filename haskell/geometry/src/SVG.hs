{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module SVG ( chart
           , paperSize
           , Group (..)
           , (<+>)
           ) where

import Prelude hiding (writeFile)
import Graphics.Svg (doctype, svg11_, with, prettyText, (<<-))
import Graphics.Svg (Element, ToElement (..))
import Graphics.Svg.Elements
import Graphics.Svg.Attributes
import Data.Complex
import Data.Text (Text, pack)
import Data.Text.Lazy.IO (writeFile)
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Point
import Circle
import Polygon
import Line


svgSize = 500
paperSize = 50

showt = pack . show

class SVGable a where
  fmtSVG :: a -> Text

instance SVGable Double where
  fmtSVG n = if n ~== 0 then "0" else toShortest n

instance SVGable CN where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y <> " "
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

------------------------------------------------------------

instance ToElement Circle where
  toElement c = circle_ [ Cx_ <<- fmtSVG x
                          , Cy_ <<- fmtSVG y
                          , R_ <<- fmtSVG (radius c)
                          , Fill_ <<- "none"
                          , Stroke_ <<- "orange"
                          , Stroke_width_ <<- "2" ]
    where (x :+ y) = center c

------------------------------------------------------------

instance ToElement Polygon where
  toElement p = polyline_ [ Points_ <<- foldMap fmtSVG pts
                          , Fill_ <<- "none"
                          , Stroke_ <<- "orange"
                          , Stroke_width_ <<- "2" ]
    where
      pts = case p of
        Polyline p -> p
        Polygon p -> p ++ [head p]

------------------------------------------------------------

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

------------------------------------------------------------

instance (Figure a, ToElement a) => ToElement (Labeled a) where
  toElement f = toElement (fromLabeled f) <>
                (text $ toElement $ getLabel f)
    where
      fontSize = 16
      textWidth = fromIntegral (length (getLabel f))
      text = text_ $ [ X_ <<- fmtSVG x
                     , Y_ <<- fmtSVG y
                     , Font_size_ <<- showt fontSize
                     , Font_family_ <<- "CMU Serif"
                     , Font_style_ <<- "italic"
                     , Stroke_ <<- "none"
                     , Fill_ <<- "white"] <> offsetX <> offsetY 
      x :+ y = labelPosition f + (dx :+ dy)
      (dx, dy) = scale (fromIntegral fontSize) (labelOffset f)
      (cx, cy) = labelCorner f
      offsetX = case signum cx of
                  -1 -> [ Text_anchor_ <<- "start" ]
                  0 -> [ Text_anchor_ <<- "middle" ]
                  1 -> [ Text_anchor_ <<- "end" ]
      offsetY = case signum cy of
                  1 -> [ Dy_ <<- showt (-fontSize `div` 4 -1) ]
                  0 -> [ Dy_ <<- showt (fontSize `div` 4 +1) ]
                  -1 -> [ Dy_ <<- showt (fontSize - 2) ]
      
------------------------------------------------------------

data Group where 
    Nil :: Group
    G :: (ToElement a, Show a, Trans a) => a -> Group
    Append :: Group -> Group -> Group

instance Semigroup Group where (<>) = Append
instance Monoid Group where mempty = Nil

infixr 5 <+>
a <+> b = G a <> G b

instance Trans Group where
    transform t Nil = Nil
    transform t (G f) = G $ transform t f 
    transform t (Append x xs) = Append (transform t x) (transform t xs)


instance Show Group where
    show Nil = mempty
    show (G a) = show a
    show (Append x xs) = show x <> show xs


instance ToElement Group where
    toElement Nil = mempty
    toElement (G a) = toElement a
    toElement (Append a b) = toElement a <> toElement b


------------------------------------------------------------

svg content =
     doctype <>
     with (svg11_ content) [ Version_ <<- "1.1"
                           , Width_ <<- "500"
                           , Height_ <<- "500"
                           , Style_ <<- "background : #444;" ]

chart :: String -> Group -> IO ()
chart name gr = writeFile name $ prettyText contents
  where
    contents = svg $ toElement $ scaler gr
    scaler = translate (svgSize/2, svgSize/2) .
             scale (svgSize/(paperSize + 1)) .
             reflect 0 

