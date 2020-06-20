{-# language OverloadedStrings #-}
import Geometry
import Data.Complex
import Data.Functor.Const

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
    aCircle
    <+> pointOn aCircle 0 #: "A"
    <+> pointOn aCircle 0.25 #: "B" 
    <+> pointOn aCircle 0.667 #: "C"

  writeSVG 500 (path <> "projectOn.svg") $
    let c = Plot  $ (\t -> t :+ sin t) . (*6)
        pA = point (1,0) #: "A"
        pB = point (2,0) #: "B"
        pC = point (4,0) #: "C"
        pD = point (6,1) #: "D"
    in  c
    <+> pA <+> pA # projectOn c #: "A'"
    <+> pB <+> pB # projectOn c #: "B'"
    <+> pC <+> pC # projectOn c #: "C'"
    <+> pD <+> pD # projectOn c #: "D'"

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

  writeSVG 300 (path <> "groups.svg") $
    group $ take 10 $ iterate (rotate 3 . scale 1.1) $ regularPoly 3

  writeSVG 300 (path <> "compose.svg") $
    let f = G . translate (1,0) . scale 0.7 . rotate 30 <>
            G . translate (1,0) . scale 0.6 . rotate (-45)
    in G aSegment # iterate f # take 8 # mconcat # rotate 90

  writeSVG 300 (path <> "beside.svg") $
    aTriangle `beside` aSquare

  writeSVG 300 (path <> "above.svg") $
    aTriangle `above` aSquare

  writeSVG 300 (path <> "serp.svg") $
    let tr t = t `above` (t `beside` t)
    in G aCircle # iterate tr # take 5 # mconcat # rotate 225 # scaleX 0.6

  writeSVG 400 (path <> "extendToLength.svg") $
   group [aSegment # rotate x # extendToLength r
         | x <- [0,5..360] 
         , let r = 2 + sin (7 * rad x) ]

  writeSVG 400 (path <> "normalTo.svg") $
   let c = Plot $ (\t -> t :+ sin t) . (*6)
   in c <+>
      group [ aSegment # at (x,0) # normalTo c
            | x <- [0,1..7] ]

    
