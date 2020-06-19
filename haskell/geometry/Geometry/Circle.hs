{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

module Geometry.Circle
  (-- * Types and classes
    Circle, Circular (..)
    -- * Constructors
  , trivialCircle, mkCircle
  ) where

import Data.Complex
import Data.Bool
import Data.Semigroup

import Geometry.Base
import Geometry.Line
import Geometry.Polygon


-- | Class for circle and decorated circle
class (Trans c, Manifold CN c, Curve c, ClosedCurve c, Figure c) =>
  Circular c where
  -- | Center of the circle.
  center :: c -> CN
  -- | Radius of the circle
  radius :: c -> Double
  -- | The angle of the starting point.
  phaseShift :: c -> Direction
  -- | The orientation of the circle (positive -- CCW).
  orientation :: c -> Direction

-- | Represents a circle with given center, radius vector and tangent direction.
data Circle = Circle CN CN Double
  deriving Show

instance Circular Circle where
  center (Circle c _ _) = c
  radius (Circle _ r _) = norm r
  orientation (Circle _ _ o) = asDeg (signum o)
  phaseShift (Circle _ r _) = angle r

-- | The trivial circle with zero radius.
trivialCircle :: Circle
trivialCircle = mkCircle 0 0

-- | The constructor for a circle with given center and radius.
-- The radius-vector has zero angle, and circle is CCW-oriented.
mkCircle :: Double -> CN -> Circle
mkCircle r c = Circle c (r:+0) 1 

instance Eq Circle where
  c1 == c2 = radius c1 ~== radius c2 &&
             center c1 ~== center c2 &&
             phaseShift c1 ~== phaseShift c2 


instance Trans Circle where
  transform t (Circle c r o) = Circle c' r' o'
    where c' = transformCN t c
          r' = transformCN t (c + r) - c'
          o' = transformOrientation t * o


instance Manifold CN Circle where
  param c t = center c + mkPolar (radius c) (rad x)
    where
      ph = turns $ phaseShift c
      x = asTurns $ ph + t * (deg (orientation c))
    
  project c p = turns (x - ph)
    where
      ph = phaseShift c
      x = orientation c * azimuth (center c) p 

  isContaining c p = distance p (center c) ~== radius c
  unit _ = 2 * pi


instance Curve Circle where
  normal c t = azimuth (center c) (c @-> t)
  tangent c t = normal c t + 90 * orientation c


instance ClosedCurve Circle where
  location c p = res
    where res | r' ~== radius c = OnCurve
              | r' < radius c   = Inside
              | r' > radius c   = Outside
          r' = distance p (center c)


instance Figure Circle where
  isTrivial c = radius c <= 0
  refPoint = center
  box cir = ((Min x1, Min y1), (Max x2, Max y2))
    where c = center cir
          r = radius cir
          x1:+y1 = c-(r:+r)
          x2:+y2 = c+(r:+r)


