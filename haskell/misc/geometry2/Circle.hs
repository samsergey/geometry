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
          w = orientation cir * transformOrientation t


instance Curve Circle where
  param c t =
    center c + mkPolar (radius c) (2*pi * orientation c *(t + phaseShift c))

  project c p =
    orientation c * (turns (asCmp (cmp p - center c)) - phaseShift c)

  isClosed = const True

  orientation (Circle _ _ o) = o

  location p c = res
    where res | r' ~== radius c = OnCurve
              | r' < radius c   = Inside
              | r' > radius c   = Outside
          r' = distance p (center c)

  unit _ = 2 * pi
  normal c t = scale (orientation c) $ azimuth (center c) (c @-> t)
  tangent c t = normal c t + asDeg (orientation c * 90)
  distanceTo p c = abs (center c `distance` p - radius c)


instance Figure Circle where
  isTrivial c = radius c < 0
  isSimilar c1 c2 = radius c1 ~== radius c2
  refPoint = center
  box cir = ((Min x1, Min y1), (Max x2, Max y2))
    where c = center cir
          r = radius cir
          x1:+y1 = c-(r:+r)
          x2:+y2 = c+(r:+r)

instance Intersections Circle Circle where
  intersections cir1 cir2 =
    filter (isContaining cir1) $
    filter (isContaining cir2) $
    invt <$> intersectionC (t cir1) (t cir2)
    where
      t = rotate (-a) . translate' (negate c1)
      invt = translate' c1 . rotate a
      c1 = center cir1
      c2 = center cir2
      a = azimuth c1 c2

intersectionC c1 c2 | d == 0 || b < 0 = []
                    | b == 0          = [x]
                    | b > 0           = [x, conjugate x]
  where
    r1 = radius c1
    r2 = radius c2
    d = magnitude (center c1 - center c2)
    a = r1**2-r2**2+d**2
    b = -(r2-r1-d)*(r2-r1+d)*(r2+r1-d)*(r2+r1+d)
    x = scale (1/(2*d)) $ a :+ sqrt b

instance Intersections Circle Line where
  intersections cir l =
    filter (isContaining cir) $
    filter (isContaining l) $
    invt <$> intersectionL (t cir) (t l)
    where
      t :: Trans x => x -> x
      t = rotate (-a) . translate' (negate (center cir))
      invt = translate' (center cir) . rotate a
      a = angle l

intersectionL c l | b > r = []
                  | b == r = [0:+b]
                  | b < r = [a:+b, (-a):+b]
  where
    b = getY (refPoint l)
    r = radius c
    a = sqrt (r**2 - b**2)
    
  
instance Intersections Line Circle where
  intersections = flip intersections

instance Intersections Circle Polygon where
  intersections c p = foldMap (intersections c) (segments p)

instance Intersections Polygon Circle where
  intersections = flip intersections

