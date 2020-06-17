{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

module Angle (
  -- * Angular class
  Angular (..)
  -- * Data types
  , Angle(..)
  ) where

import Base
import Polygon
import Circle
import Line

import Data.Complex

------------------------------------------------------------
-- | Class representing angle mark and decorated angle mark.
class (Manifold Direction an, Figure an) => Angular an where
  asAngle :: Angle -> an
  -- | Isomorphism for angles.
  toAngle :: an -> Angle

  -- | The setter for an angle.
  setValue :: Direction -> an -> an
  setValue v = asAngle . (\(Angle p s _) -> Angle p s (s + v)) . toAngle
  
  -- | The starting direction for an angle.
  --
  -- >>> anAngle 30 # rotate 45 # angleStart
  -- 45°
  --
  angleStart :: an -> Direction
  angleStart = (\(Angle _ s _) -> s) . toAngle

  -- | The final direction for an angle.
  --
  -- >>> anAngle 30 # rotate 45 # angleEnd
  -- 75°
  --
  angleEnd :: an -> Direction
  angleEnd = (\(Angle _ _ e) -> e) . toAngle
  
  -- | The value of an angle.
  --
  -- >>> anAngle 30 # rotate 45 # angleValue
  -- 30°
  --
  angleValue :: an -> Direction
  angleValue an = angleEnd an - angleStart an


------------------------------------------------------------

-- | Type representing an angle mark on the chart.
data Angle = Angle CN Direction Direction
  deriving Eq


instance AlmostEq Angle where
  a1 ~== a2 = angleValue a1 ~== angleValue a1


instance Angular Angle where
  asAngle = id
  toAngle = id


instance Show Angle where
  show an = concat ["<Angle ", val, "(", sx, " ", sy, ")>"]
    where val = show $ angleValue an
          sx = show $ getX $ refPoint an
          sy = show $ getY $ refPoint an


instance Affine Angle where
  cmp = cmp . angleStart
  asCmp x = Angle 0 (asCmp x) (asCmp x)


instance Trans Angle where
  transform t (Angle p s e) = Angle p' s' e'
    where p' = transform t p
          s' = azimuth p' (transform t (cmp p + cmp s))
          e' = azimuth p' (transform t (cmp p + cmp e))


instance Manifold Direction Angle where
  param an x = asCmp (p + cmp (asDeg (deg s + x * v)))
    where Angle p s e = asAngle an
          v = deg (angleValue an)

  project an p = rad (azimuth (refPoint an) p - angleStart an) / rad (angleValue an)

  isContaining a p = let x = project a p in 0 <= x && x <= 1 


instance Figure Angle where
  refPoint (Angle p _ _) = p
  isTrivial a = angleValue a ~== 0
  box (Angle p s e) = box $ mkCircle 0.1 p
           
------------------------------------------------------------


