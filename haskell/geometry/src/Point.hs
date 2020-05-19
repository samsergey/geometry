module Point where

import Data.Complex
import Data.Maybe

import Base

data Point = Point { pointOptions :: (LabelSettings, Style)
                   , xy :: CN }
           | Label { pointOptions :: (LabelSettings, Style)
                   , xy :: CN }

pointConstructor (Point _ _) = Point
pointConstructor (Label _ _) = Label

mkPoint :: CN -> Point
mkPoint p = Point mempty p

mkLabel :: CN -> Point
mkLabel p = Label mempty p

instance Eq Point where
  p1 == p2 = cmp p1 ~== cmp p2


instance Trans Point where
  transform t p = pointConstructor p
                  (pointOptions p)
                  (transformCN t (xy p))


instance Affine Point where
  cmp = xy
  fromCN x = Point mempty x


instance Show Point where
  show p = concat ["<", pointType, sl, " (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p
          sl = "" `fromMaybe` labelText p
          pointType = case p of
                        Point _ _ -> "Point "
                        Label _ _ -> "Label "


instance Figure Point where
  isTrivial _ = False
  isSimilar _ _ = True
  refPoint = cmp
  options = pointOptions
  setOptions o p = p { pointOptions = pointOptions p <> o }

  labelDefaults p =
    LabelSettings { getLabel = mempty
              , getLabelPosition = pure $ refPoint p
              , getLabelOffset = pure (0,1)
              , getLabelCorner = pure (0, 0)
              , getLabelAngle = pure 0 }

