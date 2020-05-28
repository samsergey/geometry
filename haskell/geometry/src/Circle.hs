{-# Language RecordWildCards #-}
module Circle
  (-- * Types and classes
    Circle (..), Circular (..)
    -- * Constructors
  , trivialCircle, mkCircle, mkCircleRC
  ) where

import Data.Complex

import Base

-- | Class for circle and decorated circle
class Curve c => Circular c where
  -- | Center of the circle.
  center :: c -> CN
  -- | Radius of the circle
  radius :: c -> Double
  -- | Orientation of the circle
  orientation :: c -> Double
  -- | The angle of the starting point.
  phaseShift :: c -> Angular
  -- | The radius-vector for a given parameter
  radiusVector :: c -> Double -> XY
  radiusVector c x = coord $ azimuth (center c) (start c)

-- | Represents a circle with given center, passing through given point.
data Circle = Circle
              CN -- ^ center,
              CN -- ^ starting point,
              Double -- ^ orientation poitive -- CW, negative -- CCW.

instance Circular Circle where
  center (Circle c _ _) =  c
  radius (Circle c p _) = distance c p  
  orientation (Circle _ _ o) = o
  phaseShift (Circle c p o) = asRad (signum o) * azimuth c p

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
  show c = concat ["<Circle ", r, ",", cn, ">"]
    where r = show $ radius c
          cn = show . coord $ center c


instance Eq Circle where
  c1 == c2 = radius c1 ~== radius c2 &&
             center c1 ~== center c2 &&
             orientation c1 ~== orientation c2 


instance Trans Circle where
  transform t cir = Circle c p w
    where c = transformCN t (center cir)
          p = transformCN t (start cir)
          p' = transformCN t (cir @-> 0.25)
          w = signum $ cross (p - c) (p' - c)


instance Curve Circle where
  param c t =
    center c + mkPolar (radius c) (2*pi*(orientation c)*(t + turns (phaseShift c)))

  project c p =
    orientation c * (turns (asCmp (cmp p - center c)) - turns (phaseShift c))

  isClosed = const True

  location p c = res
    where res | r' ~== radius c = OnCurve
              | r' < radius c   = Inside
              | r' > radius c   = Outside
          r' = distance p (center c)

  unit _ = 2 * pi
  normal c t = asCmp (c @-> t - center c)
  tangent c t = normal c t + asDeg (orientation c * 90)
  distanceTo p c = abs (center c `distance` p - radius c)


instance Figure Circle where
  isTrivial c = radius c < 0
  isSimilar c1 c2 = radius c1 ~== radius c2
  refPoint = center
  box cir = foldMap pointBox pts
    where pts = (c +) <$> [r:+0, (-r):+0, 0:+r, 0:+(-r)]
          c = center cir
          r = radius cir
