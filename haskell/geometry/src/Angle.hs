module Angle where

import Base

------------------------------------------------------------

data Angle = Angle CN Angular Angular
  deriving Eq

angleValue (Angle p s e) = e - s

angleStart (Angle _ s _) = s

angleEnd (Angle _ _ e) = e


instance Show Angle where
  show an = concat ["<Angle ", val, "(", sx, " ", sy, ")>"]
    where val = show $ angleValue an
          sx = show $ getX $ refPoint an
          sy = show $ getY $ refPoint an


instance Trans Angle where
  transform t (Angle p s e) = Angle p' s' e'
    where p' = transform t p
          s' = azimuth p' (transform t (cmp p + cmp s))
          e' = azimuth p' (transform t (cmp p + cmp e))


instance Figure Angle where
  refPoint (Angle p _ _) = p
  isTrivial a = angleValue a ~== 0
  
