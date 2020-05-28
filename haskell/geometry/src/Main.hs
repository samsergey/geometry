{-# Language TypeApplications #-}
{-# language OverloadedStrings #-}
module Main where

import Geometry

import Data.Complex

pt = point (14,5) #: "B"

lb = aLabel "A" # at (-10,0)

t = aTriangle # scaleX 1.5

c = aCircle #: "c"

l = aSegment # scale 4
    #: "a" <> white <> dotted
    # at (-1, 1) # along 35

ct' = (t <+> c) # reflectAt l 

s = aSegment # scale 8 # at' pt # normalTo l #: "s"

an = angleBetween s l #: arcs 2
        
t1 = regularPoly 5 # on c 0

main :: IO ()
main = writeSVG 450 "test.svg" $ c <+> t <+> ct' <+> l
