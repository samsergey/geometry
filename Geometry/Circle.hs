{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

module Geometry.Circle
  ( Circle, Circular (..)
  , trivialCircle, mkCircle
  ) where

import Data.Complex
import Data.Semigroup
import Geometry.Base


-- | Class for circle and decorated circle
class (Trans c, Manifold c, Curve c, ClosedCurve c, Figure c) =>
  Circular c where
  {-# MINIMAL toCircle, asCircle #-}
  toCircle :: c -> Circle
  asCircle :: Circle -> c
  
  -- | Center of the circle.
  center :: c -> Cmp
  center = center . toCircle
  
  -- | Radius of the circle
  radius :: c -> Double
  radius = radius . toCircle
  
  -- | The angle of the starting point.
  phaseShift :: c -> Direction
  phaseShift = phaseShift . toCircle

  -- | The orientation of the circle (positive -- CCW).
  orientation :: c -> Direction
  orientation = orientation . toCircle

-- | Represents a circle with given center, radius vector and tangent direction.
data Circle = Circle Cmp Cmp Double
  deriving Show

instance Circular Circle where
  toCircle = id
  asCircle = id
  center (Circle c _ _) = c
  radius (Circle _ r _) = norm r
  orientation (Circle _ _ o) = asDeg (signum o)
  phaseShift (Circle _ r _) = angle r

instance Circular c => Circular (Maybe c) where
  toCircle = maybe trivialCircle toCircle
  asCircle = Just . asCircle

-- | The trivial circle with zero radius.
trivialCircle :: Circle
trivialCircle = mkCircle 0 0

-- | The constructor for a circle with given center and radius.
-- The radius-vector has zero angle, and circle is CCW-oriented.
mkCircle :: Double -> Cmp -> Circle
mkCircle r c = Circle c (r:+0) 1 

instance Eq Circle where
  c1 == c2 = radius c1 ~= radius c2 &&
             center c1 ~= center c2 &&
             phaseShift c1 ~= phaseShift c2 


instance Trans Circle where
  transform t (Circle c r o) = Circle c' r' o'
    where c' = transformCmp t c
          r' = transformCmp t (c + r) - c'
          o' = transformOrientation t * o


instance Manifold Circle where
  type Domain Circle = Cmp
  param c t = center c + mkPolar (radius c) (rad x)
    where
      ph = turns $ phaseShift c
      x = asTurns $ ph + t * deg (orientation c)
    
  project c p = turns (x - ph)
    where
      ph = phaseShift c
      x = orientation c * azimuth (center c) p 

  isContaining c p = distance p (center c) ~= radius c
  unit _ = 2 * pi

  distanceTo p c = abs (distance p (center c) - radius c)

instance Curve Circle where
  normal c t = azimuth (center c) (c @-> t)
  tangent c t = normal c t + 90 * orientation c


instance ClosedCurve Circle where
  location c p = res
    where res | r' ~= radius c = OnCurve
              | r' < radius c  = Inside
              | r' > radius c  = Outside
          r' = distance (cmp p) (center c)


instance Figure Circle where
  isTrivial c = radius c <= 0
  refPoint = center
  box cir = ((Min x1, Min y1), (Max x2, Max y2))
    where c = center cir
          r = radius cir
          x1:+y1 = c-(r:+r)
          x2:+y2 = c+(r:+r)
