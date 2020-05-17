{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module SVG ( chart
           , paperSize, plane
           , Group (..)
           , (<+>)
           , group
           ) where

import Prelude hiding (writeFile, unwords)
import Graphics.Svg (doctype, svg11_, with, prettyText, (<<-))
import Graphics.Svg (Element, ToElement (..))
import Graphics.Svg.Elements
import Graphics.Svg.Attributes
import Data.Complex
import Data.Text (Text, pack, unwords)
import Data.Text.Lazy.IO (writeFile)
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Point
import Circle
import Polygon
import Line


svgSize = 500
paperSize = 50
plane = Polygon [(-1):+(-1), 1:+(-1), 1:+1, (-1):+1]
        <| scale (paperSize/3)
        <| rotate 30

scaled :: Trans a => a -> a
scaled = translate (svgSize/2, svgSize/2) .
         scale (svgSize/(paperSize + 1)) .
         reflect 0 

showt = pack . show

data Viewport = Viewport { size :: (Double, Double) }

class SVGable a where
  fmtSVG :: a -> Text

instance SVGable Double where
  fmtSVG n = if n ~== 0 then "0" else toShortest n

instance SVGable CN where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y <> " "
    where (x, y) = coord p

instance SVGable [CN] where
  fmtSVG pts = foldMap fmtSVG pts

instance SVGable XY where
  fmtSVG = fmtSVG . cmp


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
  toElement c' = let c = scaled c'
                     (x :+ y) = center c
    in circle_ [ Cx_ <<- fmtSVG x
               , Cy_ <<- fmtSVG y
               , R_ <<- fmtSVG (radius c)
               , Fill_ <<- "none"
               , Stroke_ <<- "orange"
               , Stroke_width_ <<- "2" ]

------------------------------------------------------------

instance ToElement Line where
  toElement l' = case l' of
    Segment _ -> let Segment (a, b) = scaled l'
      in polyline_ [ Points_ <<- fmtSVG [a,b]
                   , Fill_ <<- "none"
                   , Stroke_ <<- "orange"
                   , Stroke_width_ <<- "2" ]
    Line _ ->  foldMap toElement $ l' `clipBy` plane
    Ray _ -> foldMap toElement $ l' `clipBy` plane

------------------------------------------------------------

instance ToEleme+nt Polygon where
  toElement p' = let p = scaled p'
                     element = case p of
                       Polyline _ -> polyline_
                       Polygon _ -> polygon_
    in element [ Points_ <<- foldMap fmtSVG (vertices p)
               , Fill_ <<- "none"
               , Stroke_ <<- "orange"
               , Stroke_width_ <<- "2" ]

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
(<+>) a b = G a <> G b

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

group :: (ToElement a, Show a, Trans a) => [a] -> Group
group = foldMap G

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
    contents = svg $ toElement gr



