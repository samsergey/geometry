{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

module Geometry.Angle (
  Angular (..), Angle(..)
  ) where

import Geometry.Base
import Geometry.Polygon
import Geometry.Circle
import Geometry.Line

import Data.Complex

------------------------------------------------------------
-- | Class representing angle mark and decorated angle mark.
class (Manifold Direction an, Figure an) => Angular an where
  {-# MINIMAL asAngle, toAngle #-}
  asAngle :: Angle -> an
  -- | Isomorphism for angles.
  toAngle :: an -> Angle

  -- | The setter for an angle.
  setValue :: Direction -> an -> an
  setValue v = asAngle . setValue v . toAngle
  
  -- | The starting direction for an angle.
  --
  -- >>> anAngle 30 # rotate 45 # angleStart
  -- 45°
  --
  angleStart :: an -> Direction
  angleStart = angleStart . toAngle

  -- | The final direction for an angle.
  --
  -- >>> anAngle 30 # rotate 45 # angleEnd
  -- 75°
  --
  angleEnd :: an -> Direction
  angleEnd = angleEnd . toAngle
  
  -- | The value of an angle.
  --
  -- >>> anAngle 30 # rotate 45 # angleValue
  -- 30°
  --
  angleValue :: an -> Direction
  angleValue an = angleEnd an - angleStart an

instance Angular a => Angular (Maybe a) where
  toAngle = maybe (asCmp 0) toAngle
  asAngle = Just . asAngle
  
------------------------------------------------------------

-- | Type representing an angle mark on the chart.
data Angle = Angle CN Direction Direction
  deriving (Show, Eq)


instance AlmostEq Angle where
  a1 ~== a2 = angleValue a1 ~== angleValue a1


instance Angular Angle where
  asAngle = id
  toAngle = id
  setValue v (Angle p s _) = Angle p s (s + v)
  angleStart (Angle _ s _) = s
  angleEnd (Angle _ _ e) = e

instance Affine Angle where
  cmp = cmp . angleStart
  asCmp x = Angle 0 (asCmp x) (asCmp x)


instance Trans Angle where
  transform t (Angle p s e) = Angle p' s' e'
    where p' = transform t p
          s' = azimuth p' (transform t (cmp p + cmp s))
          e' = azimuth p' (transform t (cmp p + cmp e))


instance Manifold Direction Angle where
  param an x = asDeg (deg s + x * v)
    where Angle p s e = asAngle an
          v = deg (angleValue an)

  project an p = rad (p - angleStart an) / rad (angleValue an)

  isContaining a p = let x = project a p in 0 <= x && x <= 1 


instance Figure Angle where
  refPoint (Angle p _ _) = p
  isTrivial a = angleValue a ~== 0
  box (Angle p s e) = box $ mkCircle 0.1 p
           
------------------------------------------------------------


