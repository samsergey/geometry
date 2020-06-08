{-# Language RecordWildCards #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Circle
  (-- * Types and classes
    Circle (..), IsCircle (..)
    -- * Constructors
  , trivialCircle, mkCircle, mkCircleRC
  ) where

import Data.Complex

import Base
import Line
import Polygon
import Data.Semigroup

-- | Class for circle and decorated circle
class Curve c => IsCircle c where
  -- | Center of the circle.
  center :: c -> CN
  -- | Radius of the circle
  radius :: c -> Double
  -- | The angle of the starting point.
  phaseShift :: c -> Double
  -- | The radius-vector for a given parameter
  radiusVector :: c -> Double -> XY
  radiusVector c x = coord $ azimuth (center c) (c @-> x)

-- | Represents a circle with given center, passing through given point.
data Circle = Circle
              CN -- ^ center,
              CN -- ^ starting point,
              Double -- ^ orientation poitive -- CW, negative -- CCW.

instance IsCircle Circle where
  center (Circle c _ _) =  c
  radius (Circle c p _) = distance c p  
  phaseShift (Circle c p o) = signum o * turns (azimuth c p)

-- | The trivial circle with zero radius.
trivialCircle :: Circle
trivialCircle = Circle 0 0 1

-- | The constructor for a circle with given center and starting point.
mkCircle :: CN -> CN -> Circle
mkCircle c p = Circle c p 1

-- | The constructor for a circle with given center and radius.
-- The starting point is located at zero angle.
mkCircleRC :: Double -> CN -> Circle
mkCircleRC r c = mkCircle c (c + (r :+ 0))


instance Show Circle where
  show c = concat ["<mkCircleRC ", r, " ", cn, ">"]
    where r = show $ radius c
          cn = show $ center c


instance Eq Circle where
  c1 == c2 = radius c1 ~== radius c2 &&
             center c1 ~== center c2 &&
             orientation c1 ~== orientation c2 


instance Trans Circle where
  transform t cir = Circle c p w
    where c = transformCN t (center cir)
          p = transformCN t (start cir)
          w = orientation cir * transformOrientation t


instance Manifold Circle where
  param c t =
    center c + mkPolar (radius c) (2*pi * orientation c *(t + phaseShift c))

  project c p =
    orientation c * (turns (asCmp (cmp p - center c)) - phaseShift c)

  isClosed = const True
  isContaining c p = distance p (center c) ~== radius c
  unit _ = 2 * pi

instance Curve Circle where
  orientation (Circle _ _ o) = o

  location c p = res
    where res | r' ~== radius c = OnCurve
              | r' < radius c   = Inside
              | r' > radius c   = Outside
          r' = distance p (center c)

  normal c t = scale (orientation c) $ azimuth (center c) (c @-> t)
  tangent c t = normal c t + asDeg (orientation c * 90)

instance Figure Circle where
  isTrivial c = radius c <= 0
  isSimilar c1 c2 = radius c1 ~== radius c2
  refPoint = center
  box cir = ((Min x1, Min y1), (Max x2, Max y2))
    where c = center cir
          r = radius cir
          x1:+y1 = c-(r:+r)
          x2:+y2 = c+(r:+r)


