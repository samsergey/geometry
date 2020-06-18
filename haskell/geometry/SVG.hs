{-# language LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}

-- | This module defines representation of all figures as SVG elements. as well as default style settings.
module SVG ( -- * Classes
             Groupable, Group (..)
           , (<+>), group
           , SVGable (..), ImageSize, SVGContext(..)
           -- * Functions
           , showSVG
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
import qualified Data.Text.Lazy as LT
import Data.Double.Conversion.Text (toShortest, toPrecision)

import Base
import Decorations
import Point
import Circle
import Polygon
import Line
import Angle
import Figures

showt :: Show a => a -> Text
showt = pack . show

------------------------------------------------------------

find :: WithOptions a => (Option -> Maybe b) -> a -> Maybe b
find p d = extractOption p $ defaultOptions d <> options d

extractOption p = getFirst . foldMap (First . p) . getOptions

optFill          = \case {Fill x -> Just x; _ -> Nothing }
optStroke        = \case {Stroke x -> Just x; _ -> Nothing }
optInvisible     = \case {Invisible x -> Just x; _ -> Nothing }
optThickness     = \case {Thickness x -> Just x; _ -> Nothing }
optMultiStroke   = \case {MultiStroke x -> Just x; _ -> Nothing }
optDashing       = \case {Dashing x -> Just x; _ -> Nothing }
optLabelText     = \case {LabelText x -> Just x; _ -> Nothing }
optLabelPosition = \case {LabelPosition x -> Just x; _ -> Nothing }
optLabelOffset   = \case {LabelOffset x -> Just x; _ -> Nothing }
optLabelCorner   = \case {LabelCorner x -> Just x; _ -> Nothing }
optLabelAngle    = \case {LabelAngle x -> Just x; _ -> Nothing }
optSegmentMark   = \case {SegmentMark x -> Just x; _ -> Nothing }

------------------------------------------------------------
-- | Class for objects that could be represented as SVG elements.
-- Differs from `Graphics.Svg.ToElement` class in adding
-- parameter context to a main render function `toSVG`.
class SVGable a where
  toSVG :: a -> SVGContext -> Element
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
  toSVG d = case find optInvisible d of
    Just True -> mempty
    _ -> toSVG (fromDecorated d) . updateOptions (options d)
    

attributes :: (WithOptions f, Figure f) => f -> SVGContext -> [Attribute]
attributes f ctx = mconcat fmt . getOptions $ opts
  where
    opts = options f <> figureOptions ctx
    fmt = [ attr optStroke Stroke_
          , attr optFill Fill_
          , attr optThickness Stroke_width_
          , attr optDashing Stroke_dasharray_ ]

attr :: (Option -> Maybe String) -> AttrTag -> [Option] -> [Attribute]
attr opt a x = maybeToList $ (\s -> a <<- pack s) <$> find opt x
  where find p = getFirst . foldMap (First . p)
------------------------------------------------------------

instance SVGable Point where
  toSVG p = circle_ . attr <> labelElement p
    where    
      attr =
        attributes p <>
        const [ Cx_ <<- fmtSVG (getX p)
              , Cy_ <<- fmtSVG (getY p)
              , R_ <<- "3" ]

------------------------------------------------------------

instance SVGable Label where
  toSVG l = labelElement l . updateOptions (options l)

------------------------------------------------------------

instance SVGable Circle where
  toSVG c = circle_ . attr <> labelElement c
    where
      (x :+ y) = center c
      attr = attributes c <>
             const [ Cx_ <<- fmtSVG x
                   , Cy_ <<- fmtSVG y
                   , R_ <<- fmtSVG (radius c) ]

------------------------------------------------------------

instance SVGable Line where
  toSVG l = do
    bx <- figureBox
    let relabel s = s #: lpos ((s @-> 0.95) - cmp s) 
    foldMap (toSVG . relabel) $ l `clipBy` bx
      
instance SVGable Ray where
  toSVG r = do
    bx <- figureBox
    let relabel s = s #: lpos ((s @-> 0.95) - cmp s) 
    foldMap (toSVG . relabel) $ r `clipBy` bx

instance SVGable Segment where
  toSVG s = elem . attr <> labelElement s
    where
      (a, b) = refPoints s
      elem = if isTrivial s then mempty else line_
      attr = attributes s <>
             const [ X1_ <<- fmtSVG (getX a)
                   , Y1_ <<- fmtSVG (getY a)
                   , X2_ <<- fmtSVG (getX b)
                   , Y2_ <<- fmtSVG (getY b) ]

------------------------------------------------------------

instance SVGable Polyline where
  toSVG p | isTrivial p = labelElement p
          | otherwise = polyline_ . attr <> labelElement p
    where
      attr = attributes p <>
             const [ Points_ <<- foldMap fmtSVG (vertices p) ]

------------------------------------------------------------

instance SVGable Polygon where
  toSVG p | isTrivial p = labelElement p
          | otherwise = polygon_ . attr <> labelElement p
    where
      attr = attributes p <>
             const [ Points_ <<- foldMap fmtSVG (vertices p) ]

deriving via Polygon instance SVGable Triangle
deriving via Polygon instance SVGable Rectangle

------------------------------------------------------------

instance SVGable Angle where
  toSVG an' ctx = toSVG (rays <+> group arc) ctx' <>
                 labelElement an ctx'
    where
      an = reflex an'
      t = extractOption optLabelText (figureOptions ctx)
      label = case t of
        Just "#" -> show (angleValue an)
        Just s -> s
        Nothing -> ""
      ctx' = updateOptions (options an <> mkOptions [LabelText label]) ctx
      Just ns = extractOption optMultiStroke (figureOptions ctx')
      Angle p s e = an
      rays = mkPolyline [p + cmp s, p, p + cmp e] # scaleAt' p 20
      arc = [ plotManifold (0,1) an # at' (p + cmp s) # scaleAt' p r
            | i <- [1..ns]
            , let r = 12 + fromIntegral i * 4 ]

------------------------------------------------------------

labelElement :: (WithOptions f, Figure f) => f -> SVGContext -> Element
labelElement ff ctx = case lb of
                   Just s -> text $ toElement s
                   Nothing -> mempty
  where
    opts = figureOptions ctx
    f = Decorated (options ff <> opts, ff)

    lb = find optLabelText f
    loff = fromMaybe 0 $ find optLabelOffset f
    lpos = fromMaybe 0 $ find optLabelPosition f
    (cx, cy) = fromMaybe (0,0) $ find optLabelCorner f

    fontSize = 14
    textWidth = fromIntegral $ length lb
    text = text_ $ [ X_ <<- fmtSVG x
                   , Y_ <<- fmtSVG y
                   , Font_size_ <<- "16"
                   , Font_family_ <<- "CMU Serif"
                   , Font_style_ <<- "italic"
                   , Stroke_ <<- "none"
                   , Fill_ <<- "white"] <> offsetX <> offsetY
    
    x :+ y = lpos + d
    d = scale (fromIntegral fontSize) loff
    offsetX = case signum cx of
                -1 -> [ Text_anchor_ <<- "start" ]
                0 -> [ Text_anchor_ <<- "middle" ]
                1 -> [ Text_anchor_ <<- "end" ]
    offsetY = case signum cy of
                1 -> [ Dy_ <<- showt (-fontSize `div` 4 -1) ]
                0 -> [ Dy_ <<- showt (fontSize `div` 4 +1) ]
                -1 -> [ Dy_ <<- showt (fontSize - 2) ]

------------------------------------------------------------

-- | Constrain for an object that could be included to a group.
type Groupable a = (SVGable a, Show a, Trans a, Figure a, Eq a)

-- | The group of inhomogeneous Groupable objects.
data Group where
  EmptyFig :: Group
  G :: Groupable a => a -> Group
  Append :: Group -> Group -> Group

instance Eq Group where
  _ == _ = False

instance Semigroup Group where (<>) = Append

instance Monoid Group where mempty = EmptyFig

infixl 2 <+>
-- | The appending operator for groupable objects.
(<+>) :: (Groupable a, Groupable b) => a -> b -> Group
a <+> b = G a <> G b

instance Trans Group where
  transform t EmptyFig = EmptyFig
  transform t (G f) = G $ transform t f 
  transform t (Append x xs) = Append (transform t x) (transform t xs)


instance Show Group where
  show EmptyFig = mempty
  show (G a) = show a
  show (Append x xs) = show x <> show xs


instance SVGable Group where
  toSVG EmptyFig = mempty
  toSVG (G a) = toSVG a
  toSVG (Append a b) = toSVG a <> toSVG b


instance Figure Group where
  refPoint = left . lower . corner
  isTrivial _ = False
  box EmptyFig = mempty
  box (G f) = box f
  box (Append a b) = box a <> box b
  
-- | Returns a group of homogeneous list of objects.
group :: Groupable a => [a] -> Group
group = foldMap G

------------------------------------------------------------
-- | Type alias for explicit image settings
type ImageSize = Int

-- | The record containing image parameters and options.
data SVGContext = SVGContext
  { imageSize :: ImageSize
  , figureBox :: Rectangle
  , figureOptions :: Options }

updateOptions :: Options -> SVGContext -> SVGContext
updateOptions opts ctx = ctx {figureOptions = figureOptions ctx <> opts}

wrapSVG :: Element -> SVGContext -> Element
wrapSVG content ctx =
  doctype <>
  with (svg11_ content) 
  [ Version_ <<- "1.1"
  , Width_ <<- showt (40 + figureWidth (figureBox ctx))
  , Height_ <<- showt (40 + figureHeight (figureBox ctx))
  , Style_ <<- "background : #444;" ]

-- | Creates a SVG contents for geometric objects.
showSVG :: (Figure a, SVGable a) => ImageSize -> a -> LT.Text
showSVG size obj = prettyText (contents ctx)
  where
    contents = toSVG obj' >>= wrapSVG
    ctx = SVGContext size fb mempty
    obj' = obj
           # superpose p0 (0 :: CN)
           # reflect 0
           # scale ((fromIntegral size - 50) / ((w `max` h) `min` paperSize))
           # translate' ((25, 20) :: XY)
    fb = boxRectangle obj
         # superpose p0 (0 :: CN)
         # reflect 0
         # scale ((fromIntegral size - 20) / ((w `max` h) `min` paperSize))
         # translate' ((10, 10) :: XY)
    paperSize = 50
    w = figureWidth obj
    h = figureHeight obj
    p0 = left . upper . corner $ obj
