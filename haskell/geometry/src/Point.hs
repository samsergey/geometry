module Point where

import Data.Complex
import Data.Maybe

import Base

------------------------------------------------------------

class PointC a where
  pointOptions :: a -> Options
  xy :: a -> CN

------------------------------------------------------------

data Point = Point Options CN

mkPoint p = Point mempty (cmp p)

instance Show Point where
  show p = concat ["<Point (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

instance Eq Point where
  p1 == p2 = xy p1 ~== xy p2

instance PointC Point where
  xy (Point _ p) = p
  pointOptions (Point  o _) = o

instance Trans Point where
  transform t p = Point (pointOptions p) (transformCN t (xy p))

instance Affine Point where
  cmp = xy
  fromCN = Point mempty

instance Figure Point where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = xy
  options = pointOptions
  setOptions o' (Point o p) = Point (o <> o') p

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
    , getStrokeWidth = pure "1" }

------------------------------------------------------------

data Label = Label Options CN

mkLabel p = Label mempty (cmp p)

instance Show Label where
  show p = concat ["<Label (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

instance Eq Label where
  p1 == p2 = xy p1 ~== xy p2

instance PointC Label where
  xy (Label  _ p) = p
  pointOptions (Label  o _) = o

instance Trans Label where
  transform t p = Label (pointOptions p) (transformCN t (xy p))

instance Affine Label where
  cmp = xy
  fromCN = Label mempty

instance Figure Label where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = xy
  options = pointOptions
  setOptions o' (Label o p) = Label (o <> o') p

  labelDefaults p = LabelSettings
    { getLabel = mempty
    , getLabelPosition = pure $ xy p
    , getLabelOffset = pure (0, 0)
    , getLabelCorner = pure (0, 0)
    , getLabelAngle = pure 0 }

  styleDefaults _ = mempty

------------------------------------------------------------

