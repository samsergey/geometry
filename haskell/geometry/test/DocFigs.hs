{-# language OverloadedStrings #-}
import Geometry
import Data.Complex

main = do
  writeSVG 400 "figs/angle1.svg" $
    let t = triangle2a 30 60
        a1 = anAngle 30 #: "#" <> loffs ((-1):+1)
        a2 = anAngle 90 # on (side t 2) 0 #: "#"
        a3 = vertexAngle t 1 #: "#"
    in t <+> a1 <+> a2 <+> a3

  writeSVG 300 "figs/angle2.svg" $
    let a = anAngle 60
        b = supplementary a
    in aLine <+> aLine # rotate 60 <+>
       a #: "a" <+> b #: "b"

  writeSVG 300 "figs/angle3.svg" $
    let a = anAngle 60
        b = vertical a
    in aLine <+> aLine # rotate 60 <+>
       a #: "a" <+> b #: "b"

  writeSVG 300 "figs/angle4.svg" $
    let a = anAngle 60
        b = reflex a
    in aRay <+> aRay # rotate 60 <+>
       a #: "a" <+> b #: "b"
