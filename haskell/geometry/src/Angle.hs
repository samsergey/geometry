module Angle where

import Base

------------------------------------------------------------
-- | Type representing angle on the chart
data Angle = Angle CN Angular Angular
  deriving Eq

angleValue (Angle p s e) = e - s
setValue v (Angle p s _) = Angle p s (s + v)

angleStart (Angle _ s _) = s
angleEnd (Angle _ _ e) = e


instance Show Angle where
  show an = concat ["<Angle ", val, "(", sx, " ", sy, ")>"]
    where val = show $ angleValue an
          sx = show $ getX $ refPoint an
          sy = show $ getY $ refPoint an


instance Affine Angle where
  cmp = cmp . angleStart
  asCmp x = Angle 0 0 (asCmp x)
  

instance Trans Angle where
  transform t (Angle p s e) = Angle p' s' e'
    where p' = transform t p
          s' = azimuth p' (transform t (cmp p + cmp s))
          e' = azimuth p' (transform t (cmp p + cmp e))


instance Figure Angle where
  refPoint (Angle p _ _) = p
  isTrivial a = angleValue a ~== 0
  a1 `isSimilar` a2 = angleValue a1 ~== angleValue a2
  box (Angle p s e) = foldMap pointBox [p, p + cmp s, p + cmp e]
           
