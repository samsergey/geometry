{-# language LambdaCase #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language DerivingVia #-}
{-# language UndecidableInstances #-}
{-# language FlexibleContexts #-}

module Geometry.Decorations
  ( WithOptions (..)
  , Options, Option(..)
  , mkOptions, getOptions
  , Decorated(..), fromDecorated
  , Decorator(..)
  , (#:), (#::)
  , visible, invisible
  , stroke, white, fill
  , thickness, thin
  , dashed, dotted
  , arcs
  , label, loffs, lpos, lparam
  )
where

import Data.Monoid
import Data.Complex
import Data.Maybe
import Data.String( IsString(..) )
import Control.Monad
import Data.Functor.Identity

import Geometry.Base
import Geometry.Point
import Geometry.Line
import Geometry.Circle
import Geometry.Angle
import Geometry.Polygon
import Geometry.Plot


-- | Possible SVG options for a figure.
data Option = Invisible Bool
            | Stroke String
            | Fill String
            | Thickness String
            | Dashing String
            | MultiStroke Int
            | LabelText String
            | LabelCorner (Int, Int)
            | LabelPosition Cmp
            | LabelOffset Cmp
            | LabelAngle Direction
            | SegmentMark Int  deriving (Show)

-- | Monoidal options list wrapper. Concatenates dually to a usual list.
newtype Options = Options (Dual [Option])
  deriving (Semigroup, Monoid, Show)

-- | Puts a list of options to a wrapper.g
mkOptions = Options . Dual
-- | Extracts an option list from the `Options` wrapper.
getOptions (Options (Dual os)) = os

-- | The class of objects which could have decorations.
class WithOptions a where
  -- | Get the decoration data
  options :: a -> Options
  options = defaultOptions

  -- | Set the decoration data
  setOptions :: Options -> a -> a
  setOptions _ = id

  -- | Default labeling settings
  defaultOptions :: a -> Options
  defaultOptions _ = mempty

instance
  {-# OVERLAPPABLE #-}
  (Functor t, Foldable t, WithOptions a) => WithOptions (t a) where
  options = foldMap options
  setOptions o a = setOptions o <$> a

------------------------------------------------------------

-- | The transparent decoration wrapper for geometric objects.
-- Inherits all properties of embedded object.
newtype Decorated a = Decorated (Options, a)
  deriving Functor

-- | The selector for the embedded object.
fromDecorated (Decorated (_, x)) = x

instance Applicative Decorated where
  pure p = Decorated (mempty, p)
  (<*>) = ap

instance Monad Decorated where
  Decorated (d, x) >>= f =
    let Decorated (d', y) = f x
    in Decorated (d <> d', y)

instance {-# OVERLAPPING #-} WithOptions (Decorated a) where
  options (Decorated (o, _)) = o
  setOptions o' f = Decorated (o', id) <*> f

instance Show a => Show (Decorated a) where
  show f = l <> show (fromDecorated f)
    where l = maybe mempty (<> ":") lab
          lab = find optLabelText f
          find p d = extractOption p $ defaultOptions d <> options d
          extractOption p = getFirst . foldMap (First . p) . getOptions
          optLabelText = \case {LabelText x -> Just x; _ -> Nothing }

instance Eq a => Eq (Decorated a) where
  d1 == d2 = fromDecorated d1 == fromDecorated d2

instance Metric a => Metric (Decorated a) where
  dist a b = dist (fromDecorated a) (fromDecorated b)
  dist2 a b = dist2 (fromDecorated a) (fromDecorated b)

instance Affine a => Affine (Decorated a) where
  cmp = cmp . fromDecorated
  asCmp = pure . asCmp

instance {-# OVERLAPPING #-}  Trans a => Trans (Decorated a) where
  transform t = fmap (transform t)

instance Manifold m => Manifold (Decorated m) where
  type Domain (Decorated m) = Domain m
  param = param . fromDecorated
  project = project . fromDecorated
  paramMaybe = paramMaybe . fromDecorated
  projectMaybe = projectMaybe . fromDecorated
  isContaining = isContaining . fromDecorated
  unit = unit . fromDecorated

instance Curve c => Curve (Decorated c) where
  tangent = tangent . fromDecorated
  normal = normal . fromDecorated

instance ClosedCurve c => ClosedCurve (Decorated c) where
  isEnclosing = isEnclosing . fromDecorated
  location = location . fromDecorated

instance {-# OVERLAPPING #-} Figure a => Figure (Decorated a) where
  isTrivial = isTrivial . fromDecorated
  refPoint = refPoint . fromDecorated
  box = box . fromDecorated

instance APoint p => APoint (Decorated p) where
  toPoint = toPoint . fromDecorated
  asPoint = pure . asPoint

instance Linear l => Linear (Decorated l) where
  refPoints = refPoints . fromDecorated

instance Circular a => Circular (Decorated a) where
  toCircle = toCircle . fromDecorated
  asCircle = pure . asCircle

instance PiecewiseLinear a => PiecewiseLinear (Decorated a) where
  vertices = vertices . fromDecorated
  asPolyline = asPolyline . fromDecorated

instance Polygonal p => Polygonal (Decorated p) where

instance Angular a => Angular (Decorated a) where
  asAngle = pure . asAngle
  toAngle = toAngle . fromDecorated
  setValue v = fmap (setValue v)

instance APlot p => APlot (Decorated p) where
  rmap = fmap . rmap
  
------------------------------------------------------------
-- | A wrapped decoration function with monoidal properties,
-- corresponding to decoration options.
newtype Decorator a = Decorator (a -> Decorated a)

mkDecorator opt val = Decorator $
  \d -> setOptions (mkOptions [ opt val ]) (pure d)

instance Semigroup (Decorator a) where
  Decorator a <> Decorator b = Decorator (a >=> b)
  
instance Monoid (Decorator a) where
  mempty = Decorator $ \a -> Decorated (mempty, a)

instance WithOptions a => IsString (Decorator a) where
  fromString = label

infixl 5 #:
-- | The infix operator for decorator application.
--
-- >>> aPoint # at (4, 5) #: label "A"
-- A:<Point (4.0, 5.0)>
--
-- >>> segment (4,5) (6,9) #: "s" <> dotted <> white
-- s:<Segment (4.0,5.0) (6.0,9.0)>
--
(#:) :: WithOptions a => a -> Decorator a -> Decorated a
a #: (Decorator d) = d a

(#::) :: Decorated a -> Decorator (Decorated a) -> Decorated a
a #:: (Decorator d) = join (d a)

-- | The stroke color decorator.
stroke :: WithOptions a => String -> Decorator a
stroke = mkDecorator Stroke

-- | The decorator for white lines.
white :: WithOptions a => Decorator a
white = stroke "white"

-- | The fill color decorator.
fill :: WithOptions a => String -> Decorator a
fill = mkDecorator Fill

-- | The stroke-thickness decorator.
thickness :: WithOptions a => String -> Decorator a
thickness = mkDecorator Thickness

-- | The decorator for thin lines.
thin :: WithOptions a => Decorator a
thin = thickness "1"

-- | The decorator for dashed lines.
dashed :: WithOptions a => Decorator a
dashed = mkDecorator Dashing "5,5"

-- | The decorator for dotted lines.
dotted :: WithOptions a => Decorator a
dotted = mkDecorator Dashing "2,3"

-- | The decorator for dotted lines.
arcs :: Int -> Decorator Angle
arcs = mkDecorator MultiStroke

-- | The decorator which makes invisible object visible.
visible :: WithOptions a => Decorator a
visible = mkDecorator Invisible False

-- | The decorator which makes visible object invisible.
invisible :: WithOptions a => Decorator a
invisible = mkDecorator Invisible True

-- | The decorator for labeling objects. Could be used as overloaded string.
--
-- >>> aPoint # at (4, 5) #: "A"
-- A:<Point (4, 5)>
--
label :: WithOptions a => String -> Decorator a
label = mkDecorator LabelText

-- | The decorator for label offset.
loffs :: (Affine p,  WithOptions a) => p -> Decorator a
loffs = mkDecorator LabelOffset . conjugate . cmp

-- | The decorator for label position.
lpos :: (Affine p, WithOptions a) => p -> Decorator a
lpos = mkDecorator LabelPosition . cmp

-- | The decorator for setting label on a curve at a given parameter value.
lparam :: (WithOptions m, Manifold m) => Double -> Decorator m
lparam x = Decorator $ \f -> f #: lpos (f @-> x)

------------------------------------------------------------

