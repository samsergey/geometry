module Point where

import Data.Complex
import Data.Bool

import Base
import Decorations

data Point = Point {xy :: !CN}

mkPoint :: Affine a => a -> Point
mkPoint = Point . cmp

instance Show Point where
  show p = concat ["<Point (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p


instance Eq Point where
  p1 == p2 = xy p1 ~== xy p2


instance Trans Point where
  transform t (Point p) = Point $ transformCN t p 


instance Affine Point where
  cmp = xy
  fromCN = Point


instance Figure Point where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = xy


instance Decorated Point where
  labelDefaults p = LabelSettings
    { getLabel = mempty
    , getLabelPosition = pure $ xy p
    , getLabelOffset = pure (0, 1)
    , getLabelCorner = pure (0, 0)
    , getLabelAngle = pure 0 }

  styleDefaults _ = Style
    { getStroke = pure "#444"
    , getFill = pure "red"
    , getDashing = mempty
    , getStrokeWidth = pure "1"
    , isVisible = pure True}
