module Point where

import Data.Complex
import Data.Bool

import Base

data Point = Point { pointOptions :: Options
                   , visible :: Bool
                   , xy :: !CN }

mkPoint p = Point mempty True (cmp p)
mkLabel p = Point mempty False (cmp p)

instance Show Point where
  show p = concat ["<Point (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p

instance Eq Point where
  p1 == p2 = xy p1 ~== xy p2

instance Trans Point where
  transform t p = p { xy = transformCN t $ xy p }

instance Affine Point where
  cmp = xy
  fromCN = mkPoint

instance Figure Point where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = xy
  options = pointOptions
  setOptions o' p = p {pointOptions = pointOptions p <> o' }

  labelDefaults p = LabelSettings
    { getLabel = mempty
    , getLabelPosition = pure $ xy p
    , getLabelOffset = pure $ bool (0,0) (0,1) $ visible p
    , getLabelCorner = pure (0, 0)
    , getLabelAngle = pure 0 }

  styleDefaults _ = Style
    { getStroke = pure "#444"
    , getFill = pure "red"
    , getDashing = mempty
    , getStrokeWidth = pure "1" }
