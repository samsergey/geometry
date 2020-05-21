{-# Language RecordWildCards #-}
module Circle where

import Data.Complex

import Base

data Circle = Circle { radius :: !Double
                     , center :: !CN
                     , orientation :: !Double
                     , phaseShift :: !Double }

trivialCircle :: Circle
trivialCircle = Circle 1 0 1 0

mkCircle :: Double -> CN -> Circle
mkCircle r c = trivialCircle {radius = r, center = c }

mkCircle2 :: CN -> CN -> Circle
mkCircle2 c p = (mkCircle r c) {phaseShift = ph}
  where v = p - c
        r = magnitude v
        ph = phase v / (2*pi)


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
  transform t cir = (mkCircle2 c p) { orientation = w }
    where c = transformCN t (center cir)
          p = transformCN t (cir.@ 0)
          p' = transformCN t (cir.@ 0.25)
          w = signum $ cross (p - c) (p' - c)


instance Curve Circle where
  param Circle{..} t =
    center + mkPolar radius (2*pi*orientation*(t + phaseShift))

  locus Circle{..} p =
    orientation * (turns (asCmp (cmp p - center)) - phaseShift)

  isClosed = const True

  location p Circle{..} = res
    where res | r' ~== radius = OnCurve
              | r' < radius   = Inside
              | r' > radius   = Outside
          r' = magnitude (cmp p - center)

  unit _ = 2 * pi
  normal cir t = asCmp (cir.@ t - center cir)
  tangent cir t = normal cir t + asDeg (orientation cir * 90)
  distanceTo c p = abs (center c `distance` p - radius c)


instance Figure Circle where
  isTrivial Circle{..} = radius <= 0
  isSimilar c1 c2 = radius c1 ~== radius c2
  refPoint = center

