module Angle where

import Base
import Polygon

------------------------------------------------------------

class Figure an => Angular an where
  angleValue :: an -> Direction 
  setValue :: Direction -> an -> an
  angleStart :: an -> Direction
  angleEnd :: an -> Direction

-- | Type representing angle on the chart
data Angle = Angle CN Direction Direction
  deriving (Eq, Show)

instance AlmostEq Angle where
  a1 ~== a2 = angleValue a1 ~== angleValue a1

instance Angular Angle where
  angleValue (Angle p s e) = e - s
  setValue v (Angle p s _) = Angle p s (s + v)
  angleStart (Angle _ s _) = s
  angleEnd (Angle _ _ e) = e

--instance Show Angle where
--  show an = concat ["<Angle ", val, "(", sx, " ", sy, ")>"]
--    where val = show $ angleValue an
--          sx = show $ getX $ refPoint an
--          sy = show $ getY $ refPoint an

instance Affine Angle where
  cmp = cmp . angleStart
  asCmp x = Angle 0 (asCmp x) (asCmp x)

instance Trans Angle where
  transform t (Angle p s e) = Angle p' s' e'
    where p' = transform t p
          s' = azimuth p' (transform t (cmp p + cmp s))
          e' = azimuth p' (transform t (cmp p + cmp e))

instance Manifold Angle where
  param an x = refPoint an + cmp (angleStart an + asDeg x * angleValue an)
  project an p = rad (azimuth (refPoint an) p - angleStart an) / rad (angleValue an)
  isContaining a p = let x = project a p in 0 <= x && x <= 1 

instance Figure Angle where
  refPoint (Angle p _ _) = p
  isTrivial a = angleValue a ~== 0
  box (Angle p s e) = foldMap pointBox [p, p + cmp s, p + cmp e]
           
instance PiecewiseLinear Angle where
  vertices (Angle p s e) = [p + cmp s, p, p + cmp e]
