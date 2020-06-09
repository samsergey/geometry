{-# Language RecordWildCards #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Circle
  (-- * Types and classes
    Circle, IsCircle (..)
    -- * Constructors
  , trivialCircle, mkCircle, mkCircleRC
  ) where

import Data.Complex
import Data.Bool

import Base
import Line
import Polygon
import Data.Semigroup

-- | Class for circle and decorated circle
class Manifold c => IsCircle c where
  -- | Center of the circle.
  center :: c -> CN
  -- | Radius of the circle
  radius :: c -> Double
  -- | The angle of the starting point.
  phaseShift :: c -> Angular

-- | Represents a circle with given center, passing through given point.
newtype Circle = Circle ((CN, CN, CN), CN, Bool)

zeroPoint (Circle ((p, _, _), _, _)) = p

instance IsCircle Circle where
  center (Circle (_, c, _)) = c
  radius c = distance (center c) (zeroPoint c)  
  phaseShift c = azimuth (center c) (zeroPoint c)

-- | The trivial circle with zero radius.
trivialCircle :: Circle
trivialCircle = mkCircleRC 0 0

-- | The constructor for a circle with given center and starting point.
mkCircle :: (CN, CN, CN) -> Circle
mkCircle (p1, p2, p3)
  | isDegenerate t = Circle ((p1, p2, p3), p1, True)
  | otherwise = Circle ((p1, p2, p3), c, True)
  where
    t = mkTriangle [p1, p2, p3]
    mp1 = midPerpendicular (side t 0)
    mp2 = midPerpendicular (side t 1)
    c = head (lineIntersection mp1 mp2)


-- | The constructor for a circle with given center and radius.
-- The starting point is located at zero angle.
mkCircleRC :: Double -> CN -> Circle
mkCircleRC r c = mkCircle (c + (r :+ 0), c + (0 :+ r), c - (r :+ 0))


instance Show Circle where
  show c = concat ["<mkCircleRC ", r, " ", cn, ">"]
    where r = show $ radius c
          cn = show $ center c


instance Eq Circle where
  c1 == c2 = radius c1 ~== radius c2 &&
             center c1 ~== center c2 &&
             orientation c1 ~== orientation c2 


instance Trans Circle where
  transform t (Circle ((p1, p2, p3), o)) =
    mkCircle (p1', p2', p3') # setOrientation o
    where p1' = transformCN t p1
          p2' = transformCN t p2
          p3' = transformCN t p3


instance Manifold Circle where
  param cir t = center cir + mkPolar (radius cir) x
    where
      ph = turns $ phaseShift cir
      x = rad . asTurns $ t + ph
    
  project cir p = turns (x - ph)
    where
      ph = phaseShift cir
      x = azimuth (center cir) p 

  isContaining c p = distance p (center c) ~== radius c
  unit _ = 2 * pi


instance Oriented Circle where
  orientation (Circle (_, _, o)) = o
  setOrientation (Circle (ps, c, _)) o = Circle (ps, c, o)
  normal c t = o * azimuth (center c) (c @-> t)
    where o = bool (-1) 1 (orientation c)


instance ClosedCurve Circle where
  location c p = res
    where res | r' ~== radius c = OnCurve
              | r' < radius c   = Inside
              | r' > radius c   = Outside
          r' = distance p (center c)


instance Figure Circle where
  isTrivial c = radius c <= 0
  isSimilar c1 c2 = radius c1 ~== radius c2
  refPoint = center
  box cir = ((Min x1, Min y1), (Max x2, Max y2))
    where c = center cir
          r = radius cir
          x1:+y1 = c-(r:+r)
          x2:+y2 = c+(r:+r)


