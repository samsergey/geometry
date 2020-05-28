{-# language LambdaCase #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}

module Decorations where

import Data.Monoid
import Data.Maybe
import Data.String( IsString(..) )
import Control.Monad

import Base
import Point
import Line
import Circle
import Angle
import Polygon

data Option = Stroke String
            | Fill String
            | Thickness String
            | Dashing String
            | MultiStroke Int
            | LabelText String
            | LabelCorner (Int, Int)
            | LabelPosition CN
            | LabelOffset CN
            | LabelAngle Angular
            | SegmentMark Int  deriving (Show)

newtype Options = Options (Dual [Option])
  deriving (Semigroup, Monoid, Show)

mkOptions = Options . Dual
getOptions (Options (Dual os)) = os

-- | The class of objects which could have decorations.
class Decor a where
  -- | Get the decoration data
  options :: a -> Options
  options = defaultOptions

  -- | Set the decoration data
  setOptions :: Options -> a -> a
  setOptions _ = id

  -- | Default labeling settings
  defaultOptions :: a -> Options
  defaultOptions _ = mempty

find :: Decor a => (Option -> Maybe b) -> a -> Maybe b
find p d = extractOption p $ defaultOptions d <> options d

extractOption p = getFirst . foldMap (First . p) . getOptions

optFill          = \case {Fill x -> Just x; _ -> Nothing }
optStroke        = \case {Stroke x -> Just x; _ -> Nothing }
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

instance Decor (Decorated a) where
  options (Decorated (o, _)) = o
  setOptions o' f = Decorated (o', id) <*> f

instance Show a => Show (Decorated a) where
  show f = l <> show (fromDecorated f)
    where l = fromMaybe mempty $ (<> ":") <$> find optLabelText f

instance Eq a => Eq (Decorated a) where
  d1 == d2 = fromDecorated d1 == fromDecorated d2

instance Affine a => Affine (Decorated a) where
  cmp = cmp . fromDecorated
  asCmp = pure . asCmp

instance Trans a => Trans (Decorated a) where
  transform t = fmap (transform t)

instance Curve a => Curve (Decorated a) where
  param = param . fromDecorated
  project = project . fromDecorated
  tangent = tangent . fromDecorated
  isContaining = isContaining . fromDecorated
  isEnclosing = isEnclosing . fromDecorated
  distanceTo pt = distanceTo pt . fromDecorated

instance Figure a => Figure (Decorated a) where
  isTrivial = isTrivial . fromDecorated
  refPoint = refPoint . fromDecorated

instance Linear l => Linear (Decorated l) where
  bounding = bounding . fromDecorated
  refPoints = refPoints . fromDecorated

instance Circular a => Circular (Decorated a) where
  radius = radius . fromDecorated
  center = center . fromDecorated
  phaseShift = phaseShift . fromDecorated
  orientation = orientation . fromDecorated

instance Polygonal a => Polygonal (Decorated a) where
  vertices = vertices . fromDecorated
  polyClosed = polyClosed . fromDecorated

instance Intersections a b => Intersections a (Decorated b) where
  intersections x d = intersections x (fromDecorated d)

instance Intersections a b => Intersections (Decorated a) b where
  intersections d x = intersections (fromDecorated d) x

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

instance Decor a => IsString (Decorator a) where
  fromString = label

infixl 5 #:
-- | The infix operator for decorator apprication.
--
-- >>> aPoint # at (4, 5) #: label "A"
-- A:<Point (4.0, 5.0)>
--
-- >>> segment (4,5) (6,9) #: "s" <> dotted <> white
-- s:<Segment (4.0,5.0) (6.0,9.0)>
--
(#:) :: Decor a => a -> Decorator a -> Decorated a
a #: (Decorator d) = d a

-- | The stroke color decorator.
stroke :: Decor a => String -> Decorator a
stroke = mkDecorator Stroke

-- | The decorator for white lines.
white :: Decor a => Decorator a
white = stroke "white"

-- | The fill color decorator.
fill :: Decor a => String -> Decorator a
fill = mkDecorator Fill

-- | The stroke-width decorator.
width :: Decor a => String -> Decorator a
width = mkDecorator Thickness

-- | The decorator for thin lines.
thin :: Decor a => Decorator a
thin = width "1"

-- | The decorator for dashed lines.
dashed :: Decor a => Decorator a
dashed = mkDecorator Dashing "5,5"

-- | The decorator for dotted lines.
dotted :: Decor a => Decorator a
dotted = mkDecorator Dashing "2,3"

-- | The decorator for dotted lines.
arcs :: Int -> Decorator Angle
arcs = mkDecorator MultiStroke

-- | The decorator for labeling objects. Could be used as overloaded string.
--
-- >>> aPoint # at (4, 5) #: "A"
-- A:<Point (4, 5)>
--
label :: Decor a => String -> Decorator a
label = mkDecorator LabelText

-- | The decorator for label offset.
loffs :: Decor a => CN -> Decorator a
loffs = mkDecorator LabelOffset

-- | The decorator for label position.
lpos :: Decor a => CN -> Decorator a
lpos = mkDecorator LabelPosition

-- | The decorator for setting label on a curve at a given parameter value.
lparam :: (Curve a, Decor a) => Double -> Decorator a
lparam x = Decorator $ \f -> f #: lpos (f @-> x)
