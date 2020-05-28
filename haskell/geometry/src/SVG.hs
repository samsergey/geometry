{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

module SVG ( -- * Classes
             Group (..)
           , (<+>), group
           , SVGable (..)
           -- * Functions
           , showSVG
           -- * Parameters and constants
           , svgSize, paperSize, plane
           , attributes
           ) where

import Prelude hiding (writeFile, unwords)
import Graphics.Svg.Core (Attribute (..))
import Graphics.Svg (doctype, svg11_, with, prettyText, (<<-))
import Graphics.Svg (Element, ToElement (..))
import Graphics.Svg.Elements
import Graphics.Svg.Attributes
import Data.Complex
import Data.Monoid
import Data.Maybe
import Data.Text (Text, pack, unwords)
import Data.Text.Lazy.IO (writeFile)
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Decorations
import Point
import Circle
import Polygon
import Line
import Angle

-- |The actual size of the svg image
svgSize :: Double
svgSize = 500

-- |The virtual size of the chart paper
paperSize :: Double
paperSize = 50

-- |The curve bounding the visible paper
plane = mkPolygon [(-1,-1), (1,-1), (1,1), (-1,1) :: XY]
        # scale (paperSize/2)

showt :: Show a => a -> Text
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

attributes :: Options -> [Attribute]
attributes = mconcat fmt . getOptions
  where fmt = [ attr optStroke Stroke_
              , attr optFill Fill_
              , attr optThickness Stroke_width_
              , attr optDashing Stroke_dasharray_ ]

attr :: (Option -> Maybe String) -> AttrTag -> [Option] -> [Attribute]
attr opt a x = maybeToList $ (\s -> a <<- pack s) <$> find opt x
  where find p = getFirst . foldMap (First . p)
------------------------------------------------------------

instance Decor Point where
  defaultOptions p = mkOptions
    [ LabelPosition $ cmp p
    , LabelOffset (0 :+ 1)
    , LabelCorner (0, 0)
    , LabelAngle 0
    , Stroke "#444"
    , Fill "red"
    , Thickness "1" ]

instance SVGable Point where
  toSVG opts p = circle_ attr <> labelElement opts p
    where    
      opts' = options p <> opts
      p' = scaled p
      attr = attributes opts' <>
             [ Cx_ <<- fmtSVG (getX p')
             , Cy_ <<- fmtSVG (getY p')
             , R_ <<- "3" ]

------------------------------------------------------------

instance Decor Label where
  defaultOptions p = mkOptions
    [ LabelPosition $ cmp p
    , LabelOffset 0
    , LabelCorner (0, 0)
    , LabelAngle 0 ]

instance SVGable Label where
  toSVG opts l = labelElement (options l <> opts) l

------------------------------------------------------------

instance Decor Circle where
  defaultOptions c = mkOptions
    [ LabelPosition $ c @-> 0
    , LabelOffset $ cmp $ normal c 0.1
    , LabelCorner (-1,0)
    , LabelAngle 0
    , Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

instance SVGable Circle where
  toSVG opts c = circle_ attr <> labelElement opts c
    where
      opts' = options c <> opts
      c' = scaled c
      (x :+ y) = center c'
      attr =  attributes opts' <>
              [ Cx_ <<- fmtSVG x
              , Cy_ <<- fmtSVG y
              , R_ <<- fmtSVG (radius c') ]

------------------------------------------------------------

instance Decor Line where
  defaultOptions l = mkOptions
    [ LabelPosition $ l @-> 0.5
    , LabelOffset $ cmp $ scale 1 $ normal l 0
    , Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

instance SVGable Line where
  toSVG opts l | bounding l == Bound = elem <> labelElement opts' l
               | otherwise = foldMap (toSVG opts . relabel) $ l `clipBy` plane
    where
      opts' = options l <> opts
      (a, b) = refPoints $ scaled l
      attr = attributes opts' <>
        [ X1_ <<- fmtSVG (getX a)
        , Y1_ <<- fmtSVG (getY a)
        , X2_ <<- fmtSVG (getX b)
        , Y2_ <<- fmtSVG (getY b) ]
      elem = if isTrivial l then mempty else line_ attr
      relabel s = s #: lpos ((s @-> 0.95) - cmp s)
      
------------------------------------------------------------

instance Decor Polygon where
  defaultOptions _ = mkOptions
    [ Stroke "orange"
    , Fill "none"
    , Thickness "2" ]

instance SVGable Polygon where
  toSVG opts p
    | isTrivial p = labelElement opts p
    | otherwise = elem attr <> labelElement opts p
    where
      opts' = options p <> opts
      p' = scaled p
      elem = if isClosed p then polygon_ else polyline_
      attr = attributes opts' <>
             [ Points_ <<- foldMap fmtSVG (vertices p') ]

------------------------------------------------------------

instance Decor Triangle where
  defaultOptions = defaultOptions . fromTriangle

instance SVGable Triangle where
  toSVG opts = toSVG opts . fromTriangle

------------------------------------------------------------

instance Decor Angle where
  defaultOptions _ = mkOptions
    [ Stroke "white"
    , Fill "none"
    , Thickness "1.25"
    , MultiStroke 1]
    
instance SVGable Angle where
  toSVG opts an = toSVG opts' (poly <+> group arc) 
    where
      opts' = options an <> opts
      Just ns = extractOption optMultiStroke opts'
      poly = scaleAt p 3 $ mkPolyline [e, p, s]
      arc = [ mkPolyline [ p + scale r (cmp (asRad x))
                         | x <- [ a1, a1 + 0.05 .. a2]]
            | i <- [1..ns]
            ,  let r = 2 + fromIntegral i * 0.3 ]
      p = refPoint an
      s = p + cmp (angleStart an)
      e = p + cmp (angleEnd   an)
      a1 = rad (angleStart an) `min` rad (angleEnd an)
      a2 = rad (angleStart an) `max` rad (angleEnd an)

------------------------------------------------------------

labelElement :: (Decor f, Figure f) => Options -> f -> Element
labelElement opts ff = case lb of
                   Just s -> text $ toElement s
                   Nothing -> mempty
  where
    f = Decorated (options ff <> opts, ff)

    lb = find optLabelText f
    loff = fromMaybe 0 $ find optLabelOffset f
    lpos = fromMaybe 0 $ find optLabelPosition f
    (cx, cy) = fromMaybe (0,0) $ find optLabelCorner f

    fontSize = 16
    textWidth = fromIntegral $ length lb
    text = text_ $ [ X_ <<- fmtSVG x
                   , Y_ <<- fmtSVG y
                   , Font_size_ <<- showt fontSize
                   , Font_family_ <<- "CMU Serif"
                   , Font_style_ <<- "italic"
                   , Stroke_ <<- "none"
                   , Fill_ <<- "white"] <> offsetX <> offsetY
    
    x :+ y = scaled lpos + cmp d
    d = loff # scale (fromIntegral fontSize) # reflect 0
    offsetX = case 0*signum cx of
                -1 -> [ Text_anchor_ <<- "start" ]
                0 -> [ Text_anchor_ <<- "middle" ]
                1 -> [ Text_anchor_ <<- "end" ]
    offsetY = case 0*signum cy of
                1 -> [ Dy_ <<- showt (-fontSize `div` 4 -1) ]
                0 -> [ Dy_ <<- showt (fontSize `div` 4 +1) ]
                -1 -> [ Dy_ <<- showt (fontSize - 2) ]

------------------------------------------------------------

-- | The empty figure.
data EmptyFig = EmptyFig
  deriving (Show, Eq)

instance Trans EmptyFig where
  transform t EmptyFig = EmptyFig

instance Affine EmptyFig where
  cmp EmptyFig = 0
  asCmp _ = EmptyFig

instance SVGable EmptyFig where
  toSVG _ EmptyFig = mempty

instance Figure EmptyFig where
  refPoint _ = 0
  isTrivial _ = True
  box = mempty

------------------------------------------------------------
-- | Constrain for an object that could be included to a group.
type Groupable a = (SVGable a, Show a, Trans a, Figure a, Eq a)

-- | The group of inhomogeneous Groupable objects.
data Group where 
    G :: Groupable a => a -> Group
    Append :: Group -> Group -> Group

instance Eq Group where
  _ == _ = False

instance Semigroup Group where (<>) = Append

instance Monoid Group where mempty = G EmptyFig

infixl 5 <+>
-- | The appending operator for groupable objects.
(<+>) :: (Groupable a, Groupable b) => a -> b -> Group
a <+> b = G a <> G b

instance Trans Group where
  transform t (G f) = G $ transform t f 
  transform t (Append x xs) = Append (transform t x) (transform t xs)


instance Show Group where
  show (G a) = show a
  show (Append x xs) = show x <> show xs


instance SVGable Group where
  toSVG opts (G a) = toSVG opts a
  toSVG opts (Append a b) = toSVG opts a <> toSVG opts b


instance Figure Group where
  refPoint = bottom . left . corner
  isTrivial _ = False
  box (G f) = box f
  box (Append a b) = box a <> box b

  
-- | Returns a group of homogeneous list of objects.
group :: Groupable a => [a] -> Group
group = foldMap G

------------------------------------------------------------

svg content =
     doctype <>
     with (svg11_ content) [ Version_ <<- "1.1"
                           , Width_ <<- "500"
                           , Height_ <<- "500"
                           , Style_ <<- "background : #444;"]

-- | Creates a SVG contents for geometric objects.
showSVG obj = prettyText contents
  where
    contents = svg $ toSVG scaled mempty obj

    scaled = translate (svgSize/2, svgSize/2) .
             scale (svgSize/(paperSize + 2)) .
             reflect 0 
