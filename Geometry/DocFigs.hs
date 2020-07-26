{-# language OverloadedStrings #-}

module Geometry.DocFigs (docFigs) where

import Geometry
import System.Directory
import Control.Monad


paths = [ ".stack-work/dist/x86_64-linux-nix/Cabal-3.0.1.0/doc/html/geometry/figs/"
        , "C:\\Users\\karas\\geometry\\.stack-work\\dist\\29cc6475\\doc\\html\\geometry\\figs\\" ]

docFigs = do
  [path] <- filterM doesDirectoryExist paths
--  figs path
--  fractals path
  plots path


figs path = do
  
  writeSVG 400 (path <> "angle1.svg") $
    let t = triangle2a 30 60
        a1 = anAngle 30 #: "#" <> loffs (asDeg 135)
        a2 = anAngle 90 # on (side 2 t) 0 #: "#"
        a3 = t # vertexAngle 1 #: "#"
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
    let c  = aCircle # rotate 90
        s1 = modularScale 12 c
        t  = aTriangle # scale 2
        s2 = modularScale 9 t
    in (c <+> s1) `beside` space 0.3 `beside` (s2 <+> t)

  writeSVG 400 (path <> "linearScale.svg") $
    let p = polarPoly id [0,0.1..1]
        sc = linearScale (round . (*10)) [0,0.1..1] p
    in p <+> sc


  writeSVG 300 (path <> "points.svg") $
    aPoint #: "O" <+>
    point' (45 :: Direction) #: "A" <+>
    point' (1 :: Cmp) #: "B" 

  writeSVG 300 (path <> "pointOn.svg") $
    aCircle <+>
    pointOn aCircle 0 #: "A" <+>
    pointOn aCircle 0.25 #: "B" <+>
    pointOn aCircle 0.667 #: "C"

  writeSVG 300 (path <> "intersectionPoints.svg") $
    let p = regularPoly 7
        c = aCircle # scale 0.95
    in p <+> c <+> intersectionPoints c p

  writeSVG 400 (path <> "extendTo.svg") $
    let t = aTriangle
        s1 = aSegment # at (1,1)
        s2 = aSegment # at (0.3,0.3)
    in t <+>
       [s1 # along a # extendTo t | a <- [0,10..360] ] <+>
       [s2 # along a # extendTo t | a <- [0,10..360] ]

  writeSVG 300 (path <> "groups.svg") $
    group $ take 10 $ iterate (rotate 3 . scale 1.1) $ regularPoly 3

  writeSVG 300 (path <> "beside.svg") $
    aTriangle `beside` aSquare

  writeSVG 300 (path <> "above.svg") $
    aTriangle `above` aSquare

  writeSVG 400 (path <> "extendToLength.svg") 
    [ aSegment # rotate x # extendToLength r
    | x <- [0,5..360] 
    , let r = 2 + sin (7 * rad x) ]

  writeSVG 300 (path <> "on.svg") $
   let c = aCircle
   in c <+>
      aPoint # on c 0.1 <+>
      aSegment # scale 0.5 # on c 0.3 <+>
      aSquare # scale 0.5 # on c 0.6 <+>
      aTriangle # scale 0.5 # on c 0.9

  writeSVG 300 (path <> "through.svg") $
   let pA = point (2,3) #: "A"
       pB = point (3,2) #: "B"
   in aSegment # through' pA <+>
      aRay # through (3,2) <+>
      pA <+> pB <+> origin 

  writeSVG 300 (path <> "along.svg") $
   let a = aSegment #: "a" # at (1,0) # along 20
       t = aTriangle # along' a
       s = aSquare # at (1,1) # along' t
   in a <||> t <||> s
   
  writeSVG 300 (path <> "normalSegment.svg") $
   let t = aTriangle
   in t
      <+> t # normalSegment 0.2 #: "1"
      <+> t # normalSegment 0.5 #: "2"
      <+> t # normalSegment (2/3) #: "3"
      <+> t # normalSegment 1 #: "4"

  writeSVG 300 (path <> "heightFrom.svg") $
   let t = aTriangle
       pA = point (1,1) #: "A"
       sa = t # heightFrom pA #: "a"
       pB = point (1.2,0) #: "B"
       sb = sa # heightFrom pB #: "b"
   in t <+> pA <+> sa <+> pB <+> sb

  writeSVG 300 (path <> "midPerpendicular.svg") $
    let t = triangle2a 50 70
        l1 = t # side 0 # midPerpendicular #: thin <> white
        l2 = t # side 1 # midPerpendicular #: thin <> white
        l3 = t # side 2 # midPerpendicular #: thin <> white
        p = head $ intersectionPoints l1 l2
        c = circle' (p `distance` vertex 0 t) p
    in t <+> c <+> l1 <+> l2 <+> l3 <+> p

  writeSVG 300 (path <> "bisectrisse.svg") $
    let t = triangle2a 50 70
        r1 = t # vertexAngle 0 # bisectrisse #: thin <> white
        r2 = t # vertexAngle 1 # bisectrisse #: thin <> white
        r3 = t # vertexAngle 2 # bisectrisse #: thin <> white
        p = head $ intersectionPoints r1 r2
        c = circle' (p `distanceTo` side 0 t) p
    in t <+> r1 <+> r2 <+> r3 <+> p <+> c

  writeSVG 300 (path <> "altitude.svg") $
    let t = triangle2a 50 70
        s1 = t # altitude 0 1 #: thin <> white
        s2 = t # altitude 1 2 #: thin <> white
        s3 = t # altitude 2 3 #: thin <> white
        p = intersectionPoints s1 s2
    in t <+> s1 <+> s2 <+> s3 <+> p

  writeSVG 300 (path <> "clipBy.svg") $
    let star = polarPoly (\x -> 2 + cos (5*x)) [0,0.1..1] # closePolyline
        rs =  [ aSegment # scale 3 # rotate 35 # at (-1, x)
              | x <- [-4,-3.5..4] ]
    in star <+> foldMap (clipBy star) rs

  writeSVG 300 (path <> "polarPoly.svg") $
    polarPoly (\x -> 2 + cos (5*x)) [0,0.1..1] # closePolyline

  writeSVG 350 (path <> "triangle2a.svg")
    [ triangle2a a (150-a) #: thin | a <- [10,20..170] ]

  writeSVG 350 (path <> "triangle3s.svg")
    [ triangle3s 10 a 9 #: thin | a <- [2..18] ]

  writeSVG 350 (path <> "aRightTriangle.svg")
    [ aRightTriangle # scaleX a # scaleY (11-a) #: thin
    | a <- [0.5,1..10.5] ]

  writeSVG 350 (path <> "row.svg") $
    row [ regularPoly n | n <- [3..7] ]

  writeSVG 400 (path <> "rowSep.svg") $
    rowSep (space 1) [ regularPoly n | n <- [3..7] ]

  writeSVG 400 (path <> "vertexAngle.svg") $
    let p = regularPoly 5 # rotate 20 # scaleX 2
    in p <+> [ vertexAngle i p #: "#" | i <- [1..5] ]
  
  writeSVG 400 (path <> "altitude.svg") $
    let p = regularPoly 9 # rotate 20 # scaleX 2
    in p <+> [ p # altitude 0 i #: thin <> white <+>
               p # side i # asLine #: thin <> dashed
             | i <- [1..7] ]

  writeSVG 400 (path <> "median.svg") $
    let p = regularPoly 9 # rotate 20 # scaleX 2
    in p <+> [ p # median 0 i #: thin <> white
             | i <- [1..7] ]

plots path = do
  writeSVG 400 (path <> "normalTo.svg") $
    let p = plot (\t -> (t, sin t)) # range (0,7) #: white
    in p <+>
        [ aSegment # at (x,0) # normalTo p
        | x <- [0,0.3..7] ]

  writeSVG 300 (path <> "plot.svg") $
    let p1 = plot (\t -> (t, abs (sin t)))
        p2 = p1 # range (0, 3)
        p3 = p1 # range (0, 7)
    in p1 `above` p2 `above` p3

  writeSVG 300 (path <> "closedPlot.svg") $
    let flower t = scale (2 + sin (5*t)) (cos t, sin t)
        p = closedPlot flower # range (0, 2*pi)
    in p <||> space 1 <||> p # scaleX 0.5 # rotate 30

  writeSVG 500 (path <> "projectOn.svg") $
    let c = plot (\t -> (t, sin t)) # range (0, 6)
        pA = point (1,0) #: "A"
        pB = point (2,0) #: "B"
        pC = point (4,0) #: "C"
        pD = point (6,1) #: "D"
    in c  <+> pA <+> pA # projectOn c #: "A'" <+>
      pB <+> pB # projectOn c #: "B'" <+>
      pC <+> pC # projectOn c #: "C'" <+>
      pD <+> pD # projectOn c #: "D'"

  writeSVG 300 (path <> "flipAt.svg") $
    let p = plot (\t -> (t, sin t)) # range (0, 2*pi)
    in [ p # flipAt x #: thin | x <- [0,0.01..1] ]


fractals path = do
  writeSVG 300 (path <> "compose.svg") $
    let f = G . translate (1,0) . scale 0.7 . rotate 30 <>
            G . translate (1,0) . scale 0.6 . rotate (-45)
    in G aSegment # iterate f # take 8 # mconcat # rotate 90

  writeSVG 300 (path <> "serp.svg") $
    let tr t = t `above` (t `beside` t)
    in G aCircle # iterate tr # take 5 # mconcat # rotate 225 # scaleX 0.6
