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
import Data.Monoid
import Data.Text (Text, pack, unwords)
import Data.Text.Lazy.IO (writeFile)
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Decorations
import Point
import Circle
import Polygon
import Line


svgSize = 500
paperSize = 50
plane = mkPolygon @XY [(-1,-1), (1,-1), (1,1), (-1,1)]
        % scale (paperSize/2)

showt = pack . show
------------------------------------------------------------

class SVGable a where
  toSVG :: Options -> a -> Element
  toSVG _ _ = mempty

  fmtSVG :: a -> Text
  fmtSVG = mempty



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

instance SVGable a => SVGable (Decorated a) where
  toSVG opts d = toSVG (opts <> options d) (fromDecorated d)

attributes :: Options -> [Graphics.Svg.Core.Attribute]
attributes = attribute getStroke Stroke_ <>
             attribute getFill Fill_ <>
             attribute getStrokeWidth Stroke_width_ <>
             attribute getDashing Stroke_dasharray_
  where
    attribute getAttr attr (_, st) = 
      case getLast (getAttr st) of
        Just s -> [ attr <<- pack s ]
        Nothing -> mempty
        
------------------------------------------------------------
instance Decor Point where
  labelDefaults p = LabelSettings
    { getLabel = mempty
    , getLabelPosition = pure $ cmp p
    , getLabelOffset = pure (0, 1)
    , getLabelCorner = pure (0, 0)
    , getLabelAngle = pure 0 }

  styleDefaults _ = Style
    { getStroke = pure "#444"
    , getFill = pure "red"
    , getDashing = mempty
    , getStrokeWidth = pure "1"}

instance SVGable Point where
  toSVG opts p = circle_ attr <> labelElement opts p
    where
      p' = scaled p
      attr = attributes opts <>
             [ Cx_ <<- fmtSVG (getX p')
             , Cy_ <<- fmtSVG (getY p')
             , R_ <<- "3" ]

------------------------------------------------------------

instance Decor Label where
  labelDefaults p = LabelSettings
    { getLabel = mempty
    , getLabelPosition = pure $ cmp p
    , getLabelOffset = pure (0, 0)
    , getLabelCorner = pure (0, 0)
    , getLabelAngle = pure 0 }

instance SVGable Label where
  toSVG = labelElement

------------------------------------------------------------
instance Decor Circle where
  labelDefaults c = LabelSettings
    { getLabel = mempty
    , getLabelPosition = pure $ c .@ 0
    , getLabelOffset = pure $ coord $ normal c 0.1 
    , getLabelCorner = pure (-1,0)
    , getLabelAngle = pure 0 }
    
  styleDefaults _ = Style
    { getStroke = pure "orange"
    , getFill = pure "none"
    , getDashing = mempty
    , getStrokeWidth = pure "2" }

instance SVGable Circle where
  toSVG opts c = circle_ attr <> labelElement opts c
    where
      c' = scaled c
      (x :+ y) = center c'
      attr =  attributes opts <>
              [ Cx_ <<- fmtSVG x
              , Cy_ <<- fmtSVG y
              , R_ <<- fmtSVG (radius c') ]

------------------------------------------------------------
instance Decor Line where
  labelDefaults l = LabelSettings
    { getLabel = mempty
    , getLabelPosition = pure $ l .@ 0.5
    , getLabelOffset = pure $ coord $ scale 1 $ normal l 0
    , getLabelCorner = pure (0,0)
    , getLabelAngle = pure 0 }

  styleDefaults _ = Style
    { getStroke = pure "orange"
    , getFill = pure "none"
    , getDashing = mempty
    , getStrokeWidth = pure "2"}

instance SVGable Line where
  toSVG opts l = elem <> labelElement opts' s
    where
      (pos, s) = clip l
      opts' = ((fst opts) {getLabelPosition = pos}, snd opts)
      s' = scaled s
      (a, b) = refPoints s'
      attr = attributes opts <>
             [ X1_ <<- fmtSVG (getX a)
             , Y1_ <<- fmtSVG (getY a)
             , X2_ <<- fmtSVG (getX b)
             , Y2_ <<- fmtSVG (getY b) ]
      elem = if isTrivial s then mempty else line_ attr

      clip l = (p, s)
        where
          s = case l `clipBy` plane of
               (s:_) -> s
               [] -> trivialLine
          p = case bounding l of
            Bound -> getLabelPosition (fst opts)
            _ -> pure $ (s .@ 1) - cmp s
      
------------------------------------------------------------
instance Decor Polygon where
  styleDefaults _ = Style
    { getStroke = pure "orange"
    , getFill = pure "none"
    , getDashing = mempty
    , getStrokeWidth = pure "2" }

instance SVGable Polygon where
  toSVG opts p = elem attr <> labelElement opts p
    where
      p' = scaled p
      elem = if isClosed p then polygon_ else polyline_
      attr = attributes opts <>
             [ Points_ <<- foldMap fmtSVG (vertices p') ]

------------------------------------------------------------

labelElement :: (Decor f, Figure f) => Options -> f -> Element
labelElement opts f = case labelText f of
                        Just s -> text (toElement s)
                        Nothing -> mempty
  where
    labelOpts = fst opts
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
  toSVG opts Nil = mempty
  toSVG opts (G a) = toSVG opts a
  toSVG opts (Append a b) = toSVG opts a <> toSVG opts b


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
    contents = svg $ toSVG mempty gr

scaled :: Trans a => a -> a
scaled = translate (svgSize/2, svgSize/2) .
         scale (svgSize/(paperSize + 1)) .
         reflect 0 


