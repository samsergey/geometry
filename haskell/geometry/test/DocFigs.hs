{-# language OverloadedStrings #-}
import Geometry
import Data.Complex

path = "dist-newstyle/build/x86_64-linux/ghc-8.6.5/geometry-0.1.2.0/doc/html/geometry/figs/"

main = do
  writeSVG 400 (path <> "angle1.svg") $
    let t = triangle2a 30 60
        a1 = anAngle 30 #: "#" <> loffs ((-1):+1)
        a2 = anAngle 90 # on (side t 2) 0 #: "#"
        a3 = vertexAngle t 1 #: "#"
    in t <+> a1 <+> a2 <+> a3

  writeSVG 300 (path <> "angle2.svg") $
    let a = anAngle 60
        b = supplementary a
    in aLine <+> aLine # rotate 60 <+>
       a #: "a" <+> b #: "b"

  writeSVG 300 (path <> "angle3.svg") $
    let a = anAngle 60
        b = vertical a
    in aLine <+> aLine # rotate 60 <+>
       a #: "a" <+> b #: "b"

  writeSVG 300 (path <> "angle4.svg") $
    let a = anAngle 60
        b = reflex a
    in aRay <+> aRay # rotate 60 <+>
       a #: "a" <+> b #: "b"

  writeSVG 400 (path <> "modularScale.svg") $
    let c = aCircle # rotate 90
        s1 = group $ modularScale 12 c
        t = aTriangle # scale 2
        s2 = group $ modularScale 9 t
    in (c <+> s1) `beside` space 0.3 `beside` (s2 <+> t)

  writeSVG 300 (path <> "points.svg") $
    aPoint #: "O" <+>
    point' (45 :: Direction) #: "A" <+>
    point' (1 :: CN) #: "B" 

  writeSVG 300 (path <> "pointOn.svg") $
    aCircle <+>
    pointOn aCircle 0 #: "A" <+>
    pointOn aCircle 0.25 #: "B" <+>
    pointOn aCircle 0.667 #: "C"

  writeSVG 500 (path <> "projectOn.svg") $
    let c = Plot (\t -> (t, sin t)) (0,6) # asPolyline
        pA = point (1,0) #: "A"
        pB = point (2,0) #: "B"
        pC = point (4,0) #: "C"
        pD = point (6,1) #: "D"
    in c <+> (pA <+> pA # projectOn c #: "A'" <+>
              pB <+> pB # projectOn c #: "B'" <+>
              pB <+> pC # projectOn c #: "C'" <+>
              pD <+> pD # projectOn c #: "D'")

  writeSVG 300 (path <> "intersectionPoints.svg") $
    let p = regularPoly 7
        c = aCircle # scale 0.95
    in p <+> c <+> group (intersectionPoints c p)

  writeSVG 400 (path <> "extendTo.svg") $
    let t = aTriangle
        s1 = aSegment # at (1,1)
        s2 = aSegment # at (0.3,0.3)
    in t <+>
       group [s1 # along a # extendTo t | a <- [0,10..360] ] <+>
       group [s2 # along a # extendTo t | a <- [0,10..360] ]
