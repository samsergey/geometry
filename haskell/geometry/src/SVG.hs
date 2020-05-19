{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
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
plane = mkPolygon @XY [(-1,-1), (1,-1), (1,1), (-1,1)]
        <| scale (paperSize/3)
        <| rotate 30

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

attributes = attribute getStroke Stroke_ <>
             attribute getFill Fill_ <>
             attribute getStrokeWidth Stroke_width_ <>
             attribute getDashing Stroke_dasharray_

------------------------------------------------------------

             
instance SVGable Point 
instance ToElement Point where
  toElement p = pt <> labelElement p
    where
      pt = case p of
        Label _ _ -> mempty
        Point _ _ -> circle_ [ Cx_ <<- fmtSVG (getX p)
                             , Cy_ <<- fmtSVG (getY p)
                             , R_ <<- "3"
                             , Fill_ <<- "red"
                             , Stroke_ <<- "#444"
                             , Stroke_width_ <<- "1" ]
      
------------------------------------------------------------

instance SVGable Circle 
instance ToElement Circle where
  toElement c = let (x :+ y) = center c
    in circle_ [ Cx_ <<- fmtSVG x
               , Cy_ <<- fmtSVG y
               , R_ <<- fmtSVG (radius c)
               , Fill_ <<- "none"
               , Stroke_ <<- "orange"
               , Stroke_width_ <<- "2" ] <>
       labelElement c

------------------------------------------------------------

instance ToElement Line where
  toElement l = if isTrivial l
    then mempty
    else let (a, b) = refPoints $ scaled l
      in line_ [ X1_ <<- fmtSVG (getX a)
               , Y1_ <<- fmtSVG (getY a)
               , X2_ <<- fmtSVG (getX b)
               , Y2_ <<- fmtSVG (getY b)
               , Fill_ <<- "none"
               , Stroke_ <<- "orange"
               , Stroke_width_ <<- "2" ] <>
         labelElement l

instance SVGable Line where
  preprocess l = case l of
    Segment _ _ -> l
    l -> case l `clipBy` plane of
      (s:_) -> s <| lpos ((s .@ 1) - cmp s)
      [] -> trivialLine
      
------------------------------------------------------------

instance SVGable Polygon
instance ToElement Polygon where
  toElement p = let element = case p of
                                Polyline _ _ -> polyline_
                                Polygon _ _ -> polygon_
    in element [ Points_ <<- foldMap fmtSVG (vertices p)
               , Fill_ <<- "none"
               , Stroke_ <<- "orange"
               , Stroke_width_ <<- "2" ]<>
       labelElement p

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
    d = labelOffset f <| scale (fromIntegral fontSize) <| reflect 0
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
                           , Style_ <<- "background : #444;" ]

chart :: String -> Group -> IO ()
chart name gr = writeFile name $ prettyText contents
  where
    contents = svg . toElement . preprocess $ gr

scaled :: Trans a => a -> a
scaled = translate (svgSize/2, svgSize/2) .
         scale (svgSize/(paperSize + 1)) .
         reflect 0 


