{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module SVG ( chart
           , paperSize, plane
           , Group (..)
           , (<+>)
           , group, attributes
           ,Element, ToElement (..)
           ) where

import Prelude hiding (writeFile, unwords)
import Graphics.Svg.Core
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
plane = mkPolygon @XY [(-1,-1), (1,-1), (1,1), (-1,1)]
        % scale (paperSize/2)

showt :: Show a => a -> Text
showt = pack . show

class Show a => SVGable a where
  fmtSVG :: a -> Text
  fmtSVG = pack . show
  
  preprocess :: a -> a
  preprocess = id

instance SVGable Double where
  fmtSVG n = if n ~== 0 then "0" else toShortest n

instance SVGable CN where
  fmtSVG p = fmtSVG x <> "," <> fmtSVG y <> " "
    where (x, y) = coord p

instance SVGable [CN] where
  fmtSVG = foldMap fmtSVG

instance SVGable XY where
  fmtSVG = fmtSVG . cmp

------------------------------------------------------------

attribute getAttr attr f =
  case getStyleOption getAttr f of
    Just s -> [ attr <<- pack s ]
    Nothing -> mempty

attributes :: Figure f => f -> [Graphics.Svg.Core.Attribute]
attributes = attribute getStroke Stroke_ <>
             attribute getFill Fill_ <>
             attribute getStrokeWidth Stroke_width_ <>
             attribute getDashing Stroke_dasharray_

------------------------------------------------------------

instance SVGable Point 
instance ToElement Point where
  toElement p = elem <> labelElement p
    where
      p' = scaled p
      elem = if visible p then circle_ attr else mempty
      attr = attributes p <>
             [ Cx_ <<- fmtSVG (getX p')
             , Cy_ <<- fmtSVG (getY p')
             , R_ <<- "3" ]

------------------------------------------------------------

instance SVGable Circle 
instance ToElement Circle where
  toElement c = circle_ attr <> labelElement c
    where
      c' = scaled c
      (x :+ y) = center c'
      attr =  attributes c <>
              [ Cx_ <<- fmtSVG x
              , Cy_ <<- fmtSVG y
              , R_ <<- fmtSVG (radius c') ]

------------------------------------------------------------

instance ToElement Line where
  toElement l = elem <> labelElement l
    where
      l' = scaled l
      (a, b) = refPoints l'
      attr = attributes l <>
             [ X1_ <<- fmtSVG (getX a)
             , Y1_ <<- fmtSVG (getY a)
             , X2_ <<- fmtSVG (getX b)
             , Y2_ <<- fmtSVG (getY b) ]
      elem = if isTrivial l then mempty else line_ attr

instance SVGable Line where
  preprocess l = case bounding l of
    Bound -> l
    _ -> case l `clipBy` plane of
      (s:_) -> s % lpos ((s .@ 1) - cmp s)
      [] -> trivialLine
      
------------------------------------------------------------

instance SVGable Polygon
instance ToElement Polygon where
  toElement p = elem attr <> labelElement p
    where
      p' = scaled p
      elem = if isClosed p then polygon_ else polyline_
      attr = attributes p <>
             [ Points_ <<- foldMap fmtSVG (vertices p') ]

------------------------------------------------------------

labelElement :: Figure f => f -> Element
labelElement f = case labelText f of
                   Just s -> text (toElement s)
                   Nothing -> mempty
  where
    fontSize = 16
    Just l = labelText f
    textWidth = fromIntegral $ length l
    text = text_ $ [ X_ <<- fmtSVG x
                   , Y_ <<- fmtSVG y
                   , Font_size_ <<- showt fontSize
                   , Font_family_ <<- "CMU Serif"
                   , Font_style_ <<- "italic"
                   , Stroke_ <<- "none"
                   , Fill_ <<- "white"] <> offsetX <> offsetY 
    x :+ y = scaled (labelPosition f) + cmp d
    d = labelOffset f % scale (fromIntegral fontSize) % reflect 0
    (cx, cy) = labelCorner f
    offsetX = case 0*signum cx of
                -1 -> [ Text_anchor_ <<- "start" ]
                0 -> [ Text_anchor_ <<- "middle" ]
                1 -> [ Text_anchor_ <<- "end" ]
    offsetY = case 0*signum cy of
                1 -> [ Dy_ <<- showt (-fontSize `div` 4 -1) ]
                0 -> [ Dy_ <<- showt (fontSize `div` 4 +1) ]
                -1 -> [ Dy_ <<- showt (fontSize - 2) ]

------------------------------------------------------------

type Groupable a = (SVGable a, ToElement a, Show a, Trans a)

data Group where 
    Nil :: Group
    G :: Groupable a => a -> Group
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


instance SVGable Group where
  preprocess Nil = Nil
  preprocess (G a) = G (preprocess a)
  preprocess (Append a b) = preprocess a <> preprocess b


instance ToElement Group where
    toElement Nil = mempty
    toElement (G a) = toElement a
    toElement (Append a b) = toElement a <> toElement b


group :: Groupable a => [a] -> Group
group = foldMap G

------------------------------------------------------------

svg content =
     doctype <>
     with (svg11_ content) [ Version_ <<- "1.1"
                           , Width_ <<- "500"
                           , Height_ <<- "500"
                           , Style_ <<- "background : #444;"]

chart :: String -> Group -> IO ()
chart name gr = writeFile name $ prettyText contents
  where
    contents = svg . toElement . preprocess $ gr

scaled :: Trans a => a -> a
scaled = translate (svgSize/2, svgSize/2) .
         scale (svgSize/(paperSize + 1)) .
         reflect 0 


