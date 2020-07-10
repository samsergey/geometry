{-# language LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}
{-# language ApplicativeDo #-}

-- | This module defines representation of all figures as SVG elements. as well as default style settings.
module Geometry.SVG
  ( Groupable, Group (..) 
  , (<+>), group, row
  , (<||>), beside, above
  , SVGable (..), SVGContext(..)
  , showSVG
  ) where

import Prelude hiding (writeFile, unwords)
import Graphics.Svg.Core (Attribute (..))
import Graphics.Svg (doctype, svg11_, with, prettyText, (<<-))
import Graphics.Svg (Element, toElement)
import Graphics.Svg.Elements
import Graphics.Svg.Attributes
import Data.Complex
import Data.Monoid
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as LT
import Data.Double.Conversion.Text (toShortest)

import Geometry.Base
import Geometry.Decorations
import Geometry.Point
import Geometry.Circle
import Geometry.Polygon
import Geometry.Plot
import Geometry.Line
import Geometry.Angle
import Geometry.Figures

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
-- Differs from @Graphics.Svg.ToElement@ class in adding
-- parameter context to a main render function `toSVG`.
class SVGable a where
  toSVG :: a -> SVGContext -> Element
  toSVG _ _ = mempty

  fmtSVG :: a -> Text
  fmtSVG = mempty

instance SVGable Double where
  fmtSVG n = if n ~= 0 then "0" else toShortest n

instance SVGable Cmp where
  fmtSVG = fmtSVG . xy

instance SVGable XY where
  fmtSVG (x, y) = fmtSVG x <> "," <> fmtSVG y <> " "

instance SVGable a => SVGable (Maybe a) where
  toSVG = maybe mempty toSVG

instance SVGable a => SVGable [a] where
  toSVG = foldMap toSVG

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

    attr opt a x = maybeToList $ (\s -> a <<- pack s) <$> findOp opt x
    findOp p = getFirst . foldMap (First . p)
    
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
    foldMap (toSVG . relabel) $ clipBy bx l
      
instance SVGable Ray where
  toSVG r = do
    bx <- figureBox
    let relabel s = s #: lpos ((s @-> 0.95) - cmp s) 
    foldMap (toSVG . relabel) $ clipBy bx r 

instance SVGable Segment where
  toSVG s = element . attr <> labelElement s
    where
      (a, b) = refPoints s
      element = if isTrivial s then mempty else line_
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

instance (AlmostEq a, Pnt a) => SVGable (Plot a) where
  toSVG = toSVG . asPolyline

instance (AlmostEq a, Pnt a) => SVGable (ClosedPlot a) where
  toSVG = toSVG . asPolyline

------------------------------------------------------------

instance SVGable Angle where
  toSVG an' ctx = toSVG (rays <+> group arc) ctx' <>
                 labelElement an ctx'
    where
      an = reflex an'
      t = extractOption optLabelText (figureOptions ctx)
      labelText = case t of
        Just "#" -> show (angleValue an)
        Just v -> v
        Nothing -> ""
      ctx' = updateOptions (options an <> mkOptions [LabelText labelText]) ctx
      Just ns = extractOption optMultiStroke (figureOptions ctx')
      Angle p s e = an
      rays = mkPolyline [p + cmp s, p, p + cmp e] # scaleAt' p 20
      arc = [ plotManifold an # at' (p + cmp s) # scaleAt' p r
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
--
-- > let f = G . translate (1,0) . scale 0.7 . rotate 30 <>
-- >         G . translate (1,0) . scale 0.6 . rotate (-45)
-- > in G aSegment # iterate f # take 8 # mconcat # rotate 90
-- << figs/compose.svg >>
--
data Group where
  EmptyFig :: Group
  G :: Groupable a => a -> Group
  Append :: Group -> Group -> Group

instance Eq Group where
  _ == _ = False

instance Semigroup Group where (<>) = Append
instance Monoid Group where mempty = EmptyFig

infixl 3 <+>
-- | The appending operator for groupable objects.
(<+>) :: (Groupable a, Groupable b) => a -> b -> Group
a <+> b = G a <> G b

instance Trans Group where
  transform _ EmptyFig = EmptyFig
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
--
-- > group $ take 5 $ iterate (rotate 5 . scale 1.1) aTriangle
-- <<figs/groups.svg>>
--
group :: Groupable a => [a] -> Group
group = foldMap G

-- | Places one figure beside the other.
--
-- > aTriangle `beside` aSquare
-- <<figs/beside.svg>>
--
beside :: (Groupable a, Groupable b) => a -> b -> Group
beside f1 f2 = f1 <+> f2 # superpose (refPoint f2) (right . lower . corner $ f1)

-- | The operator form of the `beside` function.
infixl 2 <||>
(<||>) :: (Groupable a, Groupable b) => a -> b -> Group
(<||>) = beside

-- | Places one figure above the other.
--
-- > aTriangle `above` aSquare
-- <<figs/above.svg>>
--
-- > let tr t = t `above` (t `beside` t)
-- > in G aCircle # iterate tr # take 5 # mconcat # rotate 225 # scaleX 0.6
-- <<figs/serp.svg>>
--
above :: (Groupable a, Groupable b) => a -> b -> Group
above f1 f2 = f2 <+> f1 # superpose (refPoint f2) (left . upper . corner $ f2)

row fs = foldl beside EmptyFig fs
------------------------------------------------------------

-- | The record containing image parameters and options.
data SVGContext = SVGContext
  { imageSize :: Int
  , figureBox :: Rectangle
  , border :: Double
  , figureOptions :: Options }

updateOptions :: Options -> SVGContext -> SVGContext
updateOptions opts ctx = ctx {figureOptions = figureOptions ctx <> opts}

wrapSVG :: Element -> SVGContext -> Element
wrapSVG content ctx =
  doctype <>
  with (svg11_ content) 
  [ Version_ <<- "1.1"
  , Width_ <<- showt (border ctx + figureWidth (figureBox ctx))
  , Height_ <<- showt (1.5*border ctx + figureHeight (figureBox ctx))
  , Style_ <<- "background : #444;" ]

-- | Creates a SVG contents for geometric objects.
showSVG :: (Figure a, SVGable a) => Int -> a -> LT.Text
showSVG size obj = prettyText (contents ctx)
  where
    brd = 30
    contents = toSVG obj' >>= wrapSVG
    ctx = SVGContext size fb brd mempty
    obj' = obj
           # superpose p0 (0 :: Cmp)
           # reflect 0
           # scale ((fromIntegral size - brd*3) / ((w `max` h) `min` paperSize))
           # translate' ((brd*1.5, brd*1.5) :: XY)
    fb = boxRectangle obj
         # superpose p0 (0 :: Cmp)
         # reflect 0
         # scale ((fromIntegral size - brd) / ((w `max` h) `min` paperSize))
         # translate' ((brd/2, brd/2) :: XY)
    paperSize = 50
    w = figureWidth obj
    h = figureHeight obj
    p0 = left . upper . corner $ obj

------------------------------------------------------------
