module Point where

import Data.Complex
import Data.Maybe

import Base

data PointType = Point | Label | Tick
  deriving (Show, Eq)

data APoint = APoint { pointType :: PointType
                     , pointOptions :: Options
                     , xy :: CN }

mkPoint, mkLabel, mkTick :: Affine a => a -> APoint
mkPoint = APoint Point mempty . cmp
mkLabel = APoint Label mempty . cmp
mkTick = APoint Tick mempty . cmp


instance Eq APoint where
  p1 == p2 = cmp p1 ~== cmp p2


instance Trans APoint where
  transform t p = p { xy = (transformCN t (xy p)) }


instance Affine APoint where
  cmp = xy
  fromCN = APoint Point mempty


instance Show APoint where
  show p = concat ["<", show (pointType p), sl, " (", sx, " ", sy, ")>"]
    where sx = show $ getX p
          sy = show $ getY p
          sl = "" `fromMaybe` labelText p


instance Figure APoint where

  isTrivial _ = False

  isSimilar _ _ = True

  refPoint = cmp

  options = pointOptions

  setOptions o p = p { pointOptions = pointOptions p <> o }

  labelDefaults p =
    LabelSettings { getLabel = mempty
                  , getLabelPosition = pure $ refPoint p
                  , getLabelOffset = case pointType p of
                      Label -> pure (0,0)
                      Point -> pure (0,1)
                      Tick ->  pure (0,1)
                  , getLabelCorner = pure (0, 0)
                  , getLabelAngle = pure 0 }

  styleDefaults p = case pointType p of
    Point -> Style { getStroke = pure "#444"
                   , getFill = pure "red"
                   , getDashing = mempty
                   , getStrokeWidth = pure "1" }
    Tick -> Style { getStroke = pure "#444"
                   , getFill = mempty
                   , getDashing = mempty
                   , getStrokeWidth = pure "1" }
    Label -> mempty
