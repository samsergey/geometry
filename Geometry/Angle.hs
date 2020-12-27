{-# language TypeFamilies #-}
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
class (Manifold an, Figure an) => Angular an where
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
  -- <interactive>:60:2-8: error:
  --     * Variable not in scope: anAngle :: Integer -> an0
  --     * Perhaps you meant one of these:
  --         `asAngle' (line 20), data constructor `Angle' (line 59),
  --         `angle' (imported from Geometry.Base)
  --
  angleEnd :: an -> Direction
  angleEnd = angleEnd . toAngle
  
  -- | The value of an angle.
  --
  -- >>> anAngle 30 # rotate 45 # angleValue
  -- <interactive>:145:2-8: error:
  --     * Variable not in scope: anAngle :: Integer -> an0
  --     * Perhaps you meant one of these:
  --         `asAngle' (line 20), data constructor `Angle' (line 59),
  --         `angle' (imported from Geometry.Base)
  --
  angleValue :: an -> Direction
  angleValue an = angleEnd an - angleStart an

instance Angular a => Angular (Maybe a) where
  toAngle = maybe (asCmp 0) toAngle
  asAngle = Just . asAngle
  
------------------------------------------------------------

-- | Type representing an angle mark on the chart.
data Angle = Angle Cmp Direction Direction
  deriving (Show, Eq)


instance AlmostEq Angle where
  a1 ~= a2 = angleValue a1 ~= angleValue a1


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


instance Manifold Angle where
  type Domain Angle = Direction
  param an x = asDeg (deg s + x * v)
    where Angle p s e = asAngle an
          v = deg (angleValue an)

  project an p = rad (p - angleStart an) / rad (angleValue an)

  isContaining a p = let x = project a (asAffine p) in 0 <= x && x <= 1 


instance Figure Angle where
  refPoint (Angle p _ _) = p
  isTrivial a = angleValue a ~= 0
  box (Angle p s e) = box $ mkCircle 0.1 p
           
------------------------------------------------------------


