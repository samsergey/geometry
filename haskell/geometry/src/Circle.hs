{-# Language RecordWildCards #-}
module Circle where

import Data.Complex

import Base

data Circle = Circle { radius :: Double
                     , center :: CN
                     , orientation :: Double
                     , phaseShift :: Double
                     , aLabelData :: LabelData }

mkCircle r c = Circle (abs r) c 1 0 mempty

mkCircle2 c p = Circle (magnitude r) c 1 (phase r / (2*pi)) mempty
  where r = p - c

instance Show Circle where
  show Circle {..} = concat ["<Circle ", r, ",", c, ">"]
    where r = show radius
          c = show $ coord center

instance Eq Circle where
  c1 == c2 = radius c1 ~== radius c2 &&
             center c1 ~== center c2 &&
             orientation c1 ~== orientation c2 &&
             phaseShift c1 ~== phaseShift c2

instance Trans Circle where
  transform t cir = (mkCircle2 c p) {orientation = w}
    where c = transformCN t (center cir)
          p = transformCN t (cir `param` 0)
          p' = transformCN t (cir `param` 0.25)
          w = signum $ cross (p - c) (p' - c)

instance Curve Circle where
  param (Circle r c w ph _) t =
    c + mkPolar r (2*pi*w*(t + ph))

  locus (Circle _ c w ph _) p =
    w * (turns (asCmp (cmp p - c)) - ph)

  isClosed = const True

  location p (Circle r c _ _ _) = res
    where res | r' ~== r = OnCurve
              | r' < r   = Inside
              | r' > r   = Outside
          r' = magnitude (cmp p - c)

  unit _ = 2 * pi

  normal cir t = asCmp (cir `param` t - center cir)

  tangent cir t = normal cir t + asDeg (orientation cir * 90)

  distanceTo c p = abs (center c `distance` p - radius c)

instance Figure Circle where
  labelData = aLabelData
  appLabelData lb c = c {aLabelData = labelData c <> lb}
  
  isTrivial (Circle {..}) = radius <= 0

  isSimilar c1 c2 = radius c1 ~== radius c2

  refPoint = center
