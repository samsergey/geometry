{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module Circle where

import Graphics.Svg ((<<-))
import qualified Graphics.Svg as Svg
import Data.Double.Conversion.Text (toPrecision)
import Data.Complex
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Base
import Point
import Transform
import SVG

data Circle = Circle { radius :: Number
                     , center :: CXY
                     , orientation :: Number
                     , phaseShift :: Number }


instance Show Circle where
  show cir = concat ["<Circle ", show r, ",", show c, ">"]
    where r = fmtSVG $ radius cir
          c = fmtSVG $ coord $ center cir


instance Trans Circle where
  transform t cir = (mkCircle2 c p) {orientation = w}
    where c = transformCXY t (center cir)
          p = transformCXY t (cir `param` 0)
          p' = transformCXY t (cir `param` 0.25)
          w = signum $ cross (p - c) (p' - c)


instance Curve Circle where
  param (Circle r c w ph) t =
    c + mkPolar r (2*pi*w*(t + ph))

  locus (Circle _ c w ph) p =
    w * (toTurns (Vec (pos p - c)) - ph)

  isClosed = const True

  location p (Circle r c _ _) = res
    where res | r' ~== r = OnCurve
              | r' < r   = Inside
              | r' > r   = Outside
          r' = magnitude (pos p - c)

  length (Circle r c _ _) = 2 * pi * r

  normal cir t = Vec (cir `param` t - center cir)

  tangent cir t = normal cir t + Ang (orientation cir * 90)
  

instance SVGable Circle where
  toSVG c = Svg.circle_ [ Svg.Cx_ <<- fmtSVG x
                        , Svg.Cy_ <<- fmtSVG y
                        , Svg.R_ <<- fmtSVG (radius c)
                        , Svg.Fill_ <<- "none"
                        , Svg.Stroke_ <<- "orange"
                        , Svg.Stroke_width_ <<- "2" ]
    where (x :+ y) = center c

trivial = Circle 0 0 1 0
isTrivial (Circle r _ _ _) = r <= 0

mkCircle r c = Circle (abs r) c 1 0

mkCircle2 c p = Circle (magnitude r) c 1 (phase r / (2*pi))
  where r = p - c

mkCircle3 p1 p2 p3 = undefined

-- instance Monad m => Serial m Circle where
--   series = do Positive r <- limit 10 series
--               Position c <- limit 10 series 
--               return (mkCircle r c)

instance Arbitrary Circle where
  arbitrary = mkCircle <$> (abs <$> arbitrary) <*> arbitrary
  shrink (Circle r c _ _) = do r' <- shrink r
                               Position c' <- shrink (Position c)
                               return $ mkCircle r' c'
